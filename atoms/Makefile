
SRC = $(wildcard *.ml *.mli) 
ML_SRC = common.ml action.ml shape.ml faye.ml state.ml main.ml
OBJS = ${ML_SRC:.ml=.cmo}
ARGS = -package js_of_ocaml -package js_of_ocaml.syntax -ppx ppx_js -linkpkg 
OCAMLC = ocamlfind ocamlc -g -package js_of_ocaml -package lwt -ppx ppx_js

all: index.js index_master.js

%.js: %.byte
	js_of_ocaml --source-map $< --pretty

%.byte: $(OBJS) %.cmo
	$(OCAMLC) -linkpkg -o $@ $^

%.cmo: %.ml
	$(OCAMLC) -c $<

%.cmi: %.mli 
	$(OCAMLC) -c $<

clean::
	rm -f *.cm[io] *.byte index.js index_leader.js *.map .depend

# Dependencies
.depend: $(SRC) 
	ocamldep *.mli *.ml > .depend

-include .depend
