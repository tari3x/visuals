ROOT=$(shell git rev-parse --show-toplevel)

JS = index_server.js index.js index_rain.js index_load_shapes.js index_load_config.js
# RUNTIME_JS = ${JS:.js=.runtime.js}
# $(RUNTIME_JS) 

all: http_deps

include $(ROOT)/Makefile.rules

http_deps: $(HTTP_DEPS) $(JS)
