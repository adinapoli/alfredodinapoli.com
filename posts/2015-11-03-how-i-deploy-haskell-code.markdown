---
title: How I deploy Haskell Code
tags: haskell, fp, devops
---

# How I deploy Haskell Code

```
Summary: I have recently switched to build my apps using an intermediate
Docker container and then simply drop the executable on the target machine.
This has worked remarkably well.
```

Deploying Haskell code seems to be a pretty hot topic nowadays. Chatting
with people at the Haskell Exchange last October made clear everyone has his
own approach to put Haskell code into production. At work an approach
I used and worked decently was to use [Ansible](http://www.ansible.com) to
build my project to an EC2 `development` machine, then dump an AMI (Amazon
Machine Image) and reuse it across different environments. This had the
advantage of making provisioning and rollback easy (at the end of the day
you only need deploy a new AMI via the AWS' EC2 API), but has the big snag
of being quite slow if your development machine is a `tiny` instance or
similar (which is typically the case for such kind of environments), as you
need to perform a `cabal/stack` install remotely on the server.

Since switching to [stack](https://github.com/commercialhaskell/stack) as
my project builder/manager I have adopted a different approach which uses
a mixture of old and new Unix tools and - although quite simple - it's
effective. It's important to notice that this might not work for
you if you want a technique which works on **ALL** the different Linux
distros; this technique exploits FPComplete's `stack-build` Docker image,
which is based, to the best of my knowledge, on Ubuntu/Debian. Said that,
I have been able to produce executables which worked on CentOS7 out of the box.

I should also add that the following techniques might be
completely moot on Linux environments, where you **should** be using
stack's builtin `docker` feature to build your binaries. But being on Mac OS X,
and considering the quirks of `boot2docker`, I was forced to find another
solution. This is what I do these days:

* I use a `Dockerfile` for the build phase, called `Build.plan`

* I use `Build.plan` to provision a `stack-linux` executable which will
act as my local `stack` but will target the Linux environment

* I use `stack-linux` to install my project, mounting my local `$HOME/.stack`
and `$PWD/.stack-work` in the container in order to cache builds
and produce valid Unix executables.

* I use `upx` to compress the output executables to the bare minimum

* I upload the binaries & the files listed in the `data-files` section of
my `cabal` manifest on S3 in a folder called `myproject:version` (you can
use any persistent key-value store)

* I use `aws s3 sync` (you can use `rsync` if not targeting the AWS platform) to
update my development machine with the newly provided binaries & config files

* I crack on with the rest of the deployment (**NOTE**: I still need to dump
an AMI as the image needs to be used in a cluster, so YMMV)

Let's break down the points in more detail.

### Create stack-linux

The `Build.plan` looks like this:

```
FROM fpco/stack-build:lts-3.10

ADD .  /var/www/myproject
WORKDIR /var/www/myproject

CMD ["stack"]
```

I usually tend to invoke `docker` to tag this image to be my "builder", like so:

```
docker build -t myproject-builder -f Build.plan .
```

Now "creating" `stack-linux` is as easy as writing the following bash script:

```
#!/usr/bin/env bash

# You might not need the following.
$({ boot2docker shellinit; } 2>/dev/null)

docker run --rm \
       -v $HOME/path/to/my/project/myproject-dist:/root/.local \
       -v $HOME/path/to/my/project/.stack-work:/var/www/myproject/.stack-work \
       -v $HOME/.stack:/root/.stack \
       myproject-builder:latest stack $@
```

The advantage here is that we are still writing in the host filesystem, but `stack`
correctly installs the libraries in a separate folder:

```
➜  ~  ls /Users/adinapoli/work/myproject/.stack-work/install
x86_64-linux    x86_64-osx
```

### Building/installing the project

At this point we are ready to call:

```
stack-linux install
```

And let it run for a while, depending on how many dependencies your projects has. At the end,
you should have some linux binaries in the `myproject-dist` folder (have a look at the bash script
we created for `stack-linux`). The good news is that future builds will read from your local `.stack-work`
and will be much faster.

### Compressing with UPX

If all went well we should have a bunch of linux binaries in your `myproject-dist` which are already
usable on their own. I decided to go a step further (I work remotely and I live in an area with sub-par
internet connection) and compress the executables, to minimise the upload time towards S3. `upx` is a great
tool that "just works": Use it on your linux binaries and watch the size shrink down!
For my work project, which is a medium Haskell app composed of roughly 13K lines of code I was able to
get the final size down to `~9MB`. Not bad!

### Uploading binaries & data files

Finally we can tie the knot and upload on S3. I tend to use the [shelly](http://hackage.haskell.org/package/shelly)
library as my go-to tool for this kind of glue code:

``` haskell
release :: T.Text -> IO ()
release vr = shelly $ escaping False $ do
  -- If we are trying to release a version older than the current MyProject,
  -- we need to checkout the relevant tag.
  currentVersion <- liftIO extractCabalVersion
  when (currentVersion >= vr) $ do
    echo " * Older MyProject version required, checking out relevant git tag..."
    run_ "git" ["checkout", vr]

  let deployDir = "/var/www/myproject"
  run_ "./build.sh" [] -- build.sh just calls docker build as I have showed you.
  -- Find project specific files and upload them as well.
  (_, shareDir) <- T.breakOn "." . T.init <$> run "scripts/stack-linux" ["path", "--local-install-root"]
  dataFiles <- findDataFiles shareDir
  echo " * Compressing executable(s)..."
  let myExes = ["myexe1", "myexe2"] -- list here all the binaries you want to upload
  forM_ myExes $ \exe ->
    run_ "upx" ["myproject-dist/bin/" <> exe]

  echo " * Transferring compressed files to S3..."

  let releaseS3Prefix = "s3://my-s3-bucket/" <> vr
  run_ "aws" ["s3", "sync", dataFiles, releaseS3Prefix <> deployDir <> "/" <> dataFiles]

  forM_ myExes $ \exe -> do
    run_ "aws" ["s3", "cp", "myproject-dist/bin/" <> exe, releaseS3Prefix <> "/usr/bin/"]

  echo " * Done!"
  where
    findDataFiles shareDir = T.init <$>
      run "ls" ["-d"
               , shareDir <> "share/*/*"
               , "|"
               , "grep"
               , "myproject-" <> vr]
```

We essentially did the steps I already explained, with this twist:

* We searched within `.stack-work` to find any file listed in the `data-files` section of the cabal manifest
and we copied them over on S3. This is because, in my specific case, I had configuration files my exe needed
to run. Again, YMMV!

At this point your binaries (and config files) are on S3, properly versioned (I have used my project version
here). Now rolling back it's just a matter of transferring a couple of files over!

For completeness, this is an excerpt of a section of my Ansible scripts, which copies the files as we discussed:

```
- name: Install MyProject
  remote_user: service-runner
  sudo: no
  shell: aws s3 sync s3://my-s3-bucket/{{myproject_version}}/usr/bin/ /usr/local/bin/ &&
         aws s3 sync s3://my-s3-bucket/{{myproject_version}}/var/www/myproject/ /var/www/project/
```

Easy!

### Caveats and Elephants in the room
* As said, this technique is by no means universal; chances are it might not suit
you for various reasons.

* It doesn't aim to provide static executables; as you know [this is possible](https://ro-che.info/articles/2015-10-26-static-linking-ghc)
(and not difficult at all) up to a point.

* You _might_ (memory here does not help me) need to install whichever C library your executable depends upon.
For example at least one of my projects depends from `libpq`, so I had to `yum install` that on my target machine.
You don't need to worry about that when building though, thanks to the fact that `stack-build` provides you with
all you need out of the box (did I mention how great is this?)

### Conclusions

In this ocean full of DSLs, orchestrators and whatnot, I find this method simple and with these benefits:

* Easy versioning & rollbacks
* Small executables (You could potentially store them as binary blobs on a K-V store like Redis)
* Native binaries, which entails:
    - No container overhead (even if minimal)
    - Stability (on CentOS7 my experience with Docker was not the best, but this is for another post)
    - No need for a private registry
* Cached builds (I pay the compilation time only on what's really changed)
