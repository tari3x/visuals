
NAME = main
SRC = common.ml main.ml
OBJS = $(SRC: .ml = .cmo) 
ARGS = -package js_of_ocaml -package js_of_ocaml.syntax -ppx ppx_js -linkpkg 
OCAMLC = ocamlfind ocamlc -g -package js_of_ocaml -package lwt -ppx ppx_js

$(NAME).js: $(NAME).byte 
	js_of_ocaml --source-map $(NAME).byte --pretty

$(NAME).byte: $(OBJS)
	$(OCAMLC) -linkpkg -o $@ $^

%.cmo: %.ml 
	$(OCAMLC) -c $<

%.cmi: %.mli 
	$(OCAMLC) -c $<

clean::
	rm -f *.cm[io] $(NAME).byte $(NAME).js $(NAME).map

# Dependencies
.depend: | $(SRC) 
	ocamldep *.mli *.ml > .depend

-include .depend
