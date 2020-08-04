ROOT=$(shell git rev-parse --show-toplevel)

JS = index_server.js index.js index_rain.js

all: build $(JS) external start.sh stop.sh index.html

include $(ROOT)/Makefile.rules
