ROOT=$(shell git rev-parse --show-toplevel)

JS = index_server.js index.js index_rain.js index_load.js

all: build $(JS) external start.sh stop.sh index-load.html index-rain.html index.html

include $(ROOT)/Makefile.rules
