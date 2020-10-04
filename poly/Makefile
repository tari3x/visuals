ROOT=$(shell git rev-parse --show-toplevel)

OCAMLC_FLAGS = -w +a-4-40-41-42-44-45-48 

SRC=$(wildcard *.ml *.mli)

all: 
	cd $(ROOT)/poly/bin/ && make
	dune runtest
