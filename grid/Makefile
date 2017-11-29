JS = index_server.js index.js index_rain.js

all: build $(JS) external start.sh stop.sh index-server.html index.html

build:
	jbuilder build ${JS:.js=.bc.js} --dev

%.js: FORCE
	cp ../_build/default/grid/$*.bc.js $@

index.html: 
	ln -s ../html/index-client-light.html $@

index-server.html:
	ln -s ../html/$@

external:
	ln -s ../external .

start.sh:
	ln -s ../start.sh

stop.sh:
	ln -s ../stop.sh

clean:
	jbuilder clean

FORCE:
