from fabric.api import local, lcd

def publish(comment="Site Update"):
    """
    Invoke this to publish everything.
    """
    update_site()
    #commit(comment)
    push("master")

    sync_with_rsync()
    publish_to_bitbucket()


def sync_with_rsync():
    local("rsync -r _site/* ~/github/adinapoli.bitbucket.org/")
    local("rsync -avzr -e ssh --rsync-path=bin/rsync _site/* adinapoli@188.121.46.128:/home/content/24/10017624/html/")

def update_site():
    local("ghc site.hs && ./site build")

def commit(comment):
    local("git add . && git commit -m \"%s\"" % comment)

def create_tmp_folder():
    local("mkdir -p ../tmp")

def copy_site():
    local("cp -r _site/* ../tmp/")

def publish_to_bitbucket(comment="Site Update"):
    with lcd("~/github/adinapoli.bitbucket.org"):
        local("git pull")
        commit(comment)
        push("master")

def publish_to_gh_pages(comment="Site Update"):
    with lcd("../gh-pages"):
        local("rm -rf *")
        local("cp -r ../tmp/* .")
        commit(comment)
        push("gh-pages")

def push(branch):
    local("git push origin %s" % branch)
