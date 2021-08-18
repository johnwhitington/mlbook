OCaml from the Very Beginning
=============================

Cleaning up
-----------

Run ./clean


Building the PDF
----------------

source: mlbook.tex

run ./build


Kindle / HTML requirements
--------------------------

Modify `tex4ht` config to get better image resolution for bits which get
converted to images. For example on the author's machine, this file:

/usr/local/texlive/2018/texmf-dist/tex4ht/base/unix/tex4ht.env

Change all '110' to '220' to double the resolution.

HTML requires: opam install lambdasoup


Building the Kindle version
---------------------------

source: kindlemlbook.tex

run `./stage1`
run `./stage2`


Building the HTML version
-------------------------

source: pandocmlbook.tex

run `./stage1pandoc`
run `./osfhtml` to build single-file version
run `make` to build the splitter
run `./splitter` to split into multi-file version


Modifying the book
------------------

To fix an erratum, edit `mlbook.tex`. Then reflect the edit in
`kindlemlbook.tex` and `pandocmlbook.tex`. All edits must be made to all three
files, which have many surprising differences.
