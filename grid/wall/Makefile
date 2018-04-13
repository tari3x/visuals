JS = index_server.js index.js index_rain.js index_load.js

all: build $(JS) external start.sh stop.sh index-server.html index.html

ROOT=$(shell git rev-parse --show-toplevel)

# CR-someday:
# https://stackoverflow.com/questions/10451242/how-to-adjust-the-path-that-emacs-compile-goto-error-gets-from-the-compilation
build:
	echo "Entering directory '${PWD}'"
	jbuilder build ${JS:.js=.bc.js} --dev

%.js: FORCE
	cp ${ROOT}/_build/default/grid/wall/$*.bc.js $@

index.html: 
	ln -s  ${ROOT}/html/index-client-light.html $@

index-server.html:
	ln -s  ${ROOT}/html/$@

index-rain.html:
	ln -s  ${ROOT}/grid/html/$@

index-load.html:
	ln -s  ${ROOT}/grid/html/$@

external:
	ln -s  ${ROOT}/external .

start.sh:
	ln -s  ${ROOT}/start.sh

stop.sh:
	ln -s  ${ROOT}/stop.sh

clean:
	jbuilder clean

FORCE:
