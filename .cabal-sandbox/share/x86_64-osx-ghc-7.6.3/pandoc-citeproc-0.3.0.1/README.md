pandoc-citeproc
===============

This package provides a library and executable to facilitate the use of
citeproc with pandoc 1.12 and greater.  (Earlier versions of pandoc have
integrated citeproc support.)

The current version of the package includes code from citeproc-hs,
which has not been updated for some time.  When citeproc-hs is brought
up to date, this code can be removed and this package will depend
on citeproc-hs.

`pandoc-citeproc`
-----------------

The `pandoc-citeproc` executable is a filter that takes a JSON-encoded
Pandoc document, formats citations and adds a bibliography, and
returns a JSON-encoded pandoc document.

To process citations with pandoc, call pandoc-citeproc as a filter:

    pandoc --filter pandoc-citeproc input.md -s -o output.html

The bibliography will be put into a pandoc `Div` container with
class `references`.

pandoc-citeproc will look for the following metadata fields in
the input:

`bibliography`:  A path, or YAML list of paths, of bibliography
files to use.  These may be in any of the formats supported by
bibutils.

    Format            File extension
    ------------      --------------
    MODS              .mods
    BibLaTeX          .bib
    BibTeX            .bibtex
    RIS               .ris
    EndNote           .enl
    EndNote XML       .xml
    ISI               .wos
    MEDLINE           .medline
    Copac             .copac
    JSON citeproc     .json
    YAML citeproc     .yaml

    Note that the YAML bibliography should be a YAML object with a
    field `references` containing a list of YAML references.

`references`:  A YAML list of references.  Each reference is a YAML
object.  The format is essentially CSL JSON format.  Here is an example:

    - id: doe2006
      author:
        family: Doe
        given: [John, F.]
      title: Article
      page: 33-34
      issued:
        year: 2006
      type: article-journal
      volume: 6
      container-title: Journal of Generic Studies

The contents of fields will be interpreted as markdown when
appropriate:  so, for example, emphasis and strong emphasis can
be used in title fileds. Simple tex math will also be
parsed and rendered appropriately.

`csl` or `citation-style`: Path to a CSL style file.  If the file is not found
relative to the working directory, pandoc-citeproc will look in the
`$HOME/.csl` directory (or `C:\Users\USERNAME\AppData\Roaming\csl` in Windows
7).

`locale`:  Locale to use in place of the style's default locale.

`citation-abbreviations`:  Path to a CSL abbreviations JSON file. The format
is described [here](http://citationstylist.org/2011/10/19/abbreviations-for-zotero-test-release).  Here is a short example:

    { "default": {
        "container-title": {
                "Lloyd's Law Reports": "Lloyd's Rep",
                "Estates Gazette": "EG",
                "Scots Law Times": "SLT"
        }
      }
    }

The metadata must contain either `references` or `bibliography` or
both as a source of references.  `csl` and `citation-abbreviations`
are optional.  If `csl` is not provided, `chicago-author-date.csl` will be
used by default.

`pandoc-citeproc [bib2yaml|bib2json]`
-------------------------------------

If `pandoc-citeproc` is given the argument `bib2yaml` or `bib2json`,
it will not process citations. Instead, it will convert a bibliography
(either from stdin or in one or more files specified on the command
line) to a pandoc YAML metadata section or to CSL JSON suitable for
import into Zotero.  If input comes from stdin, the `-f/--format` option
must be used to specify the format of the bibliography; otherwise,
pandoc will try to determine it from the first file's extension.

This mode of `pandoc-citeproc` supersedes the old `biblio2yaml` program.

`Text.CSL.Pandoc`
-----------------

Those who use pandoc as a library (e.g. in a web application) will
need to use this module to process citations.

The module exports two functions, `processCites`, which is pure and
accepts a style and a list of references as arguments, and
`processCites'`, which lives in the IO monad and derives the style
and references from the document's metadata.

