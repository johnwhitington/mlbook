SOURCES = splitter.ml

PACKS = lambdasoup

RESULT = splitter

TARGETS := native-code

OCAMLFLAGS = -bin-annot
OCAMLNCFLAGS = -g -safe-string -w -3
OCAMLLDFLAGS = -g

all : $(TARGETS)

clean ::
	rm -rf foo foo2 *.cmt *.cmti

install : libinstall

-include OCamlMakefile
