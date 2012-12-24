from fabric.api import local, lcd

def publish(comment="Site Update"):
    """
    Invoke this to publish everything.
    """
    update_site()
    commit(comment)
    push("master")

    sync_with_rsync()

def sync_with_rsync():
    local("rsync -r _site/* ~/github/adinapoli.bitbucket.org/")
    local("rsync -avzr -e ssh --rsync-path=bin/rsync _site/* adinapoli@188.121.46.128:/home/content/24/10017624/html/")

def update_site():
    local("ghc site.hs && ./site build")

def commit(comment):
    local("git add . && git commit -m \"%s\"" % comment)

def push(branch):
    local("git push origin %s" % branch)
