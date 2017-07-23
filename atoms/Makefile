
# CR-someday: use jbuilder like bmillwood/figgie

SRC = $(wildcard *.ml *.mli) 
ML_SRC = common.ml ordered_stream.ml math.ml geometry.ml color.ml color_cycle.ml action.ml dom_wrappers.ml shape.ml faye.ml message.ml multitouch.ml global.ml state_full.ml state_light.ml gui_full.ml color_picker.ml gui_light.ml
OBJS = ${ML_SRC:.ml=.cmo}
OCAMLC = ocamlfind ocamlc -g -package js_of_ocaml -package js_of_ocaml-ppx -package js_of_ocaml-lwt -package lwt -package ppx_deriving -package ppx_compare -package base -package ppx_sexp_conv -package ppx_bin_prot
OCAMLC_FLAGS = -w +a-4-40-41-42-44-45-48 

all: index_client_light.js index_client_full.js index_server.js external/faye/client.js

external/faye/client.js:
	cp external/faye/node_modules/faye/browser/faye-browser-min.js $@

%.js: %.byte
	js_of_ocaml +base/runtime.js +weak.js --source-map $< --pretty

%.byte: $(OBJS) %.cmo
	$(OCAMLC) -linkpkg -o $@ $(EXTRA_LIBS) $^

%.cmo: %.ml
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmi: %.mli 
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

clean::
	rm -f *.cm[io] *.byte index.js index_server.js *.map .depend

# Dependencies
.depend: $(SRC) 
	ocamldep *.mli *.ml > .depend

-include .depend
