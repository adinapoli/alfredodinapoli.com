from fabric.api import local


def publish(comment="Site Update"):
    """
    Invoke this to publish everything.
    """
    update_site()
    commit(comment)
    push("master")

    create_tmp_folder()
    copy_site()
    publish_to_gh_pages()

def update_site():
    local("ghc site.hs && ./site build")

def commit(comment):
    local("git add . && git commit -m \"%s\"" % comment)

def create_tmp_folder():
    local("mkdir -p ../tmp")

def copy_site():
    local("cp _site/* ../tmp/")


def publish_to_gh_pages():
    local("cd ..&& cd gh_pages")
    local("git checkout gh_pages") # Enforce gh_pages branch switch
    local("cp -r ../tmp .")
    commit(comment)
    push("gh_pages")

def push(branch):
    local("git push origin %" % branch)
