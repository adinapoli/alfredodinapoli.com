---
title: How to backup and store your GPG private key (semi) securely
description: How to backup your GPG private key
tags: linux, security
---

```
Summary: splitting your key into multiple pieces and do something different with each piece is a not-too-shabby way to secure your GPG private key.
```

**Disclaimer: I am not a security expert, and there is no such thing as
"perfect security", thus the witty title of my blog post. You are encouraged
to try out what I am describing here, but I am not responsible for any misuse
or data loss occurring while trying out the steps below.**

I am guilty of discovering the power and versatility of GPG keys only
in my late 30s (I know, I know). It happened by pure chance as it was a 
required step for my previous job. It's a really nice way to bind and
verify digital identities to some random bits sitting in your computer.
Plus, it opens up new exciting possibilities, like securing all your
passwords with something like [pass](https://www.passwordstore.org/). 
However, this has the (expected) consequence that now your GPG key becomes
a single point of failure and if it gets lost or, worse, stolen, it can be
quite a disaster. 

To mitigate this, saving it somewhere safe is paramount. This begs the
question: "how?". I usually have (enough but not too much) trust in cloud
providers like Dropbox, Google Drive & co, but ultimately you are sending
your precious data over the network (hoping the connection is secure and
the traffic not spoofed) and if one of these services gets
compromised, the integrity and security of your data also is.

To mitigate this, I have been researching ways of securely storing my
private GPG key, and I think what I came up with is decent (at least for my
use case).

## First attempt

My first attempt was - as suggested around - to use something like `qrencode` 
to turn my GPG key into a QR code I could then print and store in a
closet. Then I could use something like [zbarimg](http://manpages.ubuntu.com/manpages/bionic/man1/zbarimg.1.html) to retrive my key. However,
this approach releaved to be not practical as my key is too big and it doesn't
fit into a QR code.

## The final solution

I ended up splitting my key into 4 parts, and I have _then encoded as a QR code
only the first part_; the other three have been encrypted with a symmetric key
(i.e. `gpg -c your-file`). By doing that, I can store the encrypted parts
inside Dropbox, and print the first part and store it physically in a safe
place.

## Why?

That's a fair question; in principle, simply encrypting my GPG key with a 
symmetric key and upload it to Dropbox might have been enough, but I feel like
this schema opens up more possibilities. I especially like the fact I am
not dependant on just one cloud provider and that, even if Dropbox is ever
compromised, and even if by some quantum-computer-miracle my symmetric key is
bruteforced, the attacker would still miss the last piece of the puzzle (no
pun intended).

Last but not least, here I have only used a single symmetric key for all the
three parts of my key, but nobody is preventing me from using two or even
three different ones. This way, even if one symmetric key is leaked somehow,
the attacker couldn't still have access to my full GPG key (provided he could
get his hands on the QR code, of course).

## The scripts

I have two scripts that I have whipped up: the first one exports my GPG key
and split it into chunks:

```
#!/usr/bin/env bash

# First export your gpg key like so:
#
# gpg --export-secret-keys -a -o mykey.asc
#
# Then this script will generate 4 qr codes for your key. At this point
# it's up to you what to do with these images.

split -b 2800 $1 mykey-

for file in mykey-??; do
    <"$file" qrencode -o "$file".png
done
```

Running this script giving as input the filename of your exported key
would first split it into multiple parts and finally for each of them
run `qrencode`.  At this point you would be left with an `.asc` file
(which you might want to delete now) and a bunch of files like, for example:

```
mykey-aa
mykey-aa.png
mykey-ab
mykey-ab.png
...
```

At this point you can delete all the `.png` but the first one, and conversely
delete the first `mykey-aa` keeping the other around. Now, for each of the
"chunks" (except the one and only `.png`, of course) proceed to the gpg
encryption with a symmetric key of your choice. For example:

```
gpg -c mykey-ab
```

This will spawn pinetry and allow you to insert the password. At the end of
this operation, you should have multiple `.gpg` files: these are the ones you
can store on Dropbox. Your files should be looking somewhat like this:

```
mykey-aa.png
mykey-ab.gpg
mykey-ac.gpg
mykey-ad.gpg
```

### Reconstructing the key

Once the time will come to reconstruct your key, you can use this other script:

```
#!/usr/bin/env bash

RESULT=mykey.asc

zbarimg --raw $1-aa.png | perl -pe 'chomp if eof' - > $RESULT

for f in $1-ab.gpg $1-ac.gpg $1-ad.gpg; do
  echo $f;
  gpg -dq $f >> $RESULT;
done

# Sanity check
gpg --dearmor $RESULT >/dev/null
```

You have to supply to this script the `basename` of your files (in our
example would be `mykey`) and the script would first read the QR code, then
decrypt the other chunks (in my case they are just 3 and are hardcoded in the
script, your mileage might vary) and _dulcis in fundo_ it will perform a sanity
check on the reconstructed key.

Voilà, your key has been reconstructed! You can now import the key again 
inside your GPG keychain by doing `gpg --import <key>`.

I have tested this approach myself when migrating the key from a laptop to
another and it worked just fine, so hopefully this will be useful to you as
well.

# Other alternatives

Somebody also suggested to use something like [paperbak](https://github.com/Rupan/paperbak),
which is ultimately possible but I was put off by what seemed to be quite a
careful procedure for restoring the data from the printed bitmap. In particular
the readme explains how is important to have a good printer etc etc, so I
didn't want to take the risk.
