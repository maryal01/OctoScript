all : toplevel.native print.o

clean: 
	ocamlbuild -clean
	rm -rf parser.mli parser.output parser.ml ocamlllvm *.diff

toplevel.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind toplevel.native -package llvm

print : print.c
	cc -o print print.c