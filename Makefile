all : toplevel.native test.o

clean: 
	ocamlbuild -clean
	rm -rf parser.mli parser.output parser.ml ocamlllvm *.diff *.o *.output *.err *.log

toplevel.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind toplevel.native

test : test.c
	cc -o test -DBUILD_TEST test.c
