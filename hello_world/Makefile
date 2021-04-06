all : toplevel.native

clean: 
	ocamlbuild -clean
	rm -rf parser.mli parser.output parser.ml ocamlllvm *.diff *.o *.output

toplevel.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind toplevel.native
