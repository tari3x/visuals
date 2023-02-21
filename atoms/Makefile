OCAMLC_FLAGS = -w +a-4-40-41-42-44-45-48 

JS = index_client_light.js index_client_full.js index_server.js

all: build $(JS) index.html external start.sh stop.sh external/faye/client.js

build:
	dune build ${JS:.js=.bc.js}

%.js: FORCE
	cp ../_build/default/atoms/$*.bc.js $@

index.html: 
	ln -s ../html/index-client-light.html $@

external:
	ln -s ../external .

start.sh:
	ln -s ../start.sh

stop.sh:
	ln -s ../stop.sh

external/faye/client.js:
	cp external/faye/node_modules/faye/browser/faye-browser-min.js $@

clean:
	dune clean

FORCE:
