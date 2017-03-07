
SRC = $(wildcard *.ml *.mli) 
ML_SRC = ../atoms/common.ml ../atoms/math.ml ../atoms/geometry.ml ../atoms/color.ml ../atoms/action.ml ../atoms/dom_wrappers.ml calibrate.ml index.ml
OBJS = ${ML_SRC:.ml=.cmo}
ARGS = -package js_of_ocaml -package js_of_ocaml.syntax -ppx ppx_js -linkpkg 
OCAMLC = ocamlfind ocamlc -g -package js_of_ocaml -package lwt -ppx ppx_js -I ../atoms
OCAMLC_FLAGS = -w +a-4-40-41-42-44-45-48 

EXTRA_JS = ../atoms/dom_wrappers_raw.js
EXTRA_LIBS = unix.cma

all: index.js

%.js: $(EXTRA_JS) %.byte
	js_of_ocaml --source-map $^ --pretty --disable genprim

%.byte: $(OBJS) %.cmo
	$(OCAMLC) -g -linkpkg -o $@ $(EXTRA_LIBS) $^

%.cmo: %.ml
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmi: %.mli 
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

clean::
	rm -f *.cm[io] *.byte index.js *.map .depend

# Dependencies
.depend: $(SRC) 
	ocamldep *.mli *.ml ../atoms/*.ml ../atoms/*.mli > .depend

include .depend
