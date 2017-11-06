
# CR-someday: use jbuilder, this will make the js_of_ocaml step incremental and
# faster. See bmillwood/figgie.

SRC = $(wildcard *.ml *.mli) 
ML_SRC = ../atoms/common.ml ../atoms/math.ml ../atoms/geometry.ml ../atoms/color.ml ../atoms/action.ml ../atoms/dom_wrappers.ml three.ml three_wrappers.ml prism.ml index.ml
OBJS = ${ML_SRC:.ml=.cmo}
OCAMLC = ocamlfind ocamlc -g -package js_of_ocaml -package js_of_ocaml-ppx -package js_of_ocaml-lwt -package lwt -package ppx_deriving -package ppx_compare -package base -package ppx_sexp_conv -package ppx_bin_prot -I ../atoms
OCAMLC_FLAGS = -w +a-4-40-41-42-44-45-48 

EXTRA_JS = ../atoms/dom_wrappers_raw.js

all: index.js

%.js: $(EXTRA_JS) %.byte
	js_of_ocaml +base/runtime.js +weak.js --source-map $^ --pretty --disable genprim

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
