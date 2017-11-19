OCAMLC_FLAGS = -w +a-4-40-41-42-44-45-48 

JS = index.js

all: build $(JS) external start.sh stop.sh

build:
	jbuilder build ${JS:.js=.bc.js}

%.js: FORCE
	cp ../_build/default/calibrate/$*.bc.js $@

external:
	ln -s ../external .

start.sh:
	ln -s ../start.sh

stop.sh:
	ln -s ../stop.sh

clean:
	jbuilder clean

FORCE:
