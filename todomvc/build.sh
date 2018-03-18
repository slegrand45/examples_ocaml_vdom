#!/bin/sh

# Compile OCaml source file to OCaml bytecode
ocamlfind ocamlc \
	-package js_of_ocaml,js_of_ocaml.ppx,ppx_deriving,js_of_ocaml-ppx_deriving_json,js_of_ocaml.deriving,ocaml-vdom \
	-no-check-prims -linkpkg \
	-o todomvc.byte todomvc.ml

# Build JS code from the OCaml bytecode
js_of_ocaml +gen_js_api/ojs_runtime.js --opt 3 -o js/todomvc.js todomvc.byte
