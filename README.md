mlbook
======

Cleaning up
-----------

Run ./clean

Building the PDF
----------------

source: mlbook.tex
run ./build


Kindle / epub / HTML requirements
---------------------------------

1. Needs tex4ht (provided with your tex distribution)
2. Modify tex4ht config to get better image resolution.

Building the Kindle version
---------------------------

source: kindlemlbook.tex

run ./stage1
run ./stage2

Building the ePub version
-------------------------

source: pandocmlbook.tex
run ./stage1pandoc
run ./epub

Building the HTML version
-------------------------

source: pandocmlbook.tex
run ./stage1pandoc
run ./osfhtml
run ./htmlsplit

Modifying the book
------------------

To fix a typo, edit mlbook.tex. Then reflect the edit in kindlemlbook.tex and pandocmlbook.tex. All edits must be made to all three files.

