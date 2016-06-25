
SRC = $(wildcard *.ml *.mli) 
ML_SRC = common.ml math.ml geometry.ml color.ml color_cycle.ml dom_wrappers.ml action.ml shape.ml faye.ml multitouch.ml state.ml main.ml
OBJS = ${ML_SRC:.ml=.cmo}
ARGS = -package js_of_ocaml -package js_of_ocaml.syntax -ppx ppx_js -linkpkg 
OCAMLC = ocamlfind ocamlc -g -package js_of_ocaml -package lwt -ppx ppx_js
OCAMLC_FLAGS = -w +a-4-40-41-42-44-45-48 

EXTRA_LIBS = unix.cma

all: index.js index_master.js

%.js: %.byte
	js_of_ocaml --source-map $< --pretty

%.byte: $(OBJS) %.cmo
	$(OCAMLC) -linkpkg -o $@ $(EXTRA_LIBS) $^

%.cmo: %.ml
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmi: %.mli 
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

clean::
	rm -f *.cm[io] *.byte index.js index_leader.js *.map .depend

# Dependencies
.depend: $(SRC) 
	ocamldep *.mli *.ml > .depend

-include .depend
