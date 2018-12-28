OCAMLC_FLAGS = -w +a-4-40-41-42-44-45-48 

SRC=$(wildcard *.ml *.mli)

all: a.exe

a.exe: FORCE
	jbuilder build main.exe --dev --display quiet
	cp ../_build/default/bin/main.exe $@

clean:
	jbuilder clean

FORCE:
