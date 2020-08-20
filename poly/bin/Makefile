ROOT=$(shell git rev-parse --show-toplevel)

OCAMLC_FLAGS = -w +a-4-40-41-42-44-45-48 

SRC=$(wildcard *.ml *.mli)

ROOT=${shell git rev-parse --show-toplevel}

all: a.exe index.js

a.exe index.js: FORCE
	dune build main.exe main.bc.js --display quiet 
	cp ${ROOT}/_build/default/poly/bin/main.exe a.exe
	cp ${ROOT}/_build/default/poly/bin/main.bc.js index.js

FORCE:

include $(ROOT)/Makefile.rules
