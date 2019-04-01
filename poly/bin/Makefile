OCAMLC_FLAGS = -w +a-4-40-41-42-44-45-48 

SRC=$(wildcard *.ml *.mli)

ROOT=${shell git rev-parse --show-toplevel}

all: a.exe

a.exe: FORCE
	dune build main.exe --display quiet 
	cp ${ROOT}/_build/default/poly/bin/main.exe $@

clean:
	dune clean

FORCE:
