OCAMLC_FLAGS = -w +a-4-40-41-42-44-45-48 

SRC=$(wildcard *.ml *.mli)

all: a.exe

a.exe: ${SRC}
	jbuilder build main.exe --dev
	cp _build/default/main.exe $@

clean:
	jbuilder clean

FORCE:
