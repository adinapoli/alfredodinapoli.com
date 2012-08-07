from fabric.api import local, lcd


def publish(comment="Site Update"):
    """
    Invoke this to publish everything.
    """
    update_site()
    commit(comment)
    push("master")

    create_tmp_folder()
    copy_site()
    publish_to_gh_pages(comment)

def update_site():
    local("ghc site.hs && ./site build")

def commit(comment):
    local("git add . && git commit -m \"%s\"" % comment)

def create_tmp_folder():
    local("mkdir -p ../tmp")

def copy_site():
    local("cp -r _site/* ../tmp/")


def publish_to_gh_pages(comment="Site Update"):
    with lcd("../gh-pages"):
        local("rm -rf *")
        local("cp -r ../tmp/* .")
        commit(comment)
        push("gh-pages")

def push(branch):
    local("git push origin %s" % branch)
