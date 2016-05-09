
ARGS = -package js_of_ocaml -package js_of_ocaml.syntax -ppx ppx_js -linkpkg 

main.js: main.ml
	ocamlfind ocamlc $< -o main.byte $(ARGS)
	js_of_ocaml main.byte
