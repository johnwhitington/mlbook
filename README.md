OCaml from the Very Beginning
=============================

Note: unusually, this repository contains both the source and the built
artefacts. You only need to build if you update something.

Cleaning up
-----------

Run ./clean


Building the PDF
----------------

source: mlbook.tex

run ./build


Building the Kindle version
---------------------------

(KDP no longer accept .mobi files, so this is redundant now, in favour of the
.epub process)

Modify `tex4ht` config to get better image resolution for bits which get
converted to images. For example on the author's machine, this file:

/usr/local/texlive/2021/texmf-dist/tex4ht/base/unix/tex4ht.env

Change all '110' to '220' to double the resolution.

(No, there is no way to configure this properly. You really do need to edit
this file.)

source: kindlemlbook.tex

requires: ebook-convert from Calibre

run `./stage1`

run `./stage2`


Building the Epub version
-------------------------

source: pandocmlbook.tex

requires: pandoc

run ./stage1pandoc

run ./epub


Building the HTML version
-------------------------

source: pandocmlbook.tex

requires: pandoc and opam install lambdasoup

run `./stage1pandoc`

run `./osfhtml` to build single-file version ocamlfromtheverybeginning.html

run `make` to build the splitter

run `./splitter` to split into multi-file version in html/index.html


Modifying the book
------------------

To fix errata, edit `mlbook.tex`. Then reflect the edit in `kindlemlbook.tex`
and `pandocmlbook.tex`. All edits must be made to all three files, which have
many surprising differences. [2022: kindlemlbook.tex now defunct since KDP now
requires epub files].
