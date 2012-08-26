from fabric.api import local, lcd

def publish(comment="Site Update"):
    """
    Invoke this to publish everything.
    """
    update_site()
    commit(comment)
    push("master")

    sync_with_rsync()
    publish_to_bitbucket()


def sync_with_rsync():
    local("rsync -r _site/* ~/Dropbox/Apps/Pancake.io/")
    local("rsync -r _site/* ~/github/adinapoli/")
    local("rsync -r _site/* ~/Dropbox/Apps/KISSr/adinapoli.kissr.com/")

def update_site():
    local("ghc site.hs && ./site build")

def commit(comment):
    local("git add . && git commit -m \"%s\"" % comment)

def create_tmp_folder():
    local("mkdir -p ../tmp")

def copy_site():
    local("cp -r _site/* ../tmp/")

def publish_to_bitbucket(comment="Site Update"):
    with lcd("~/github/adinapoli"):
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
