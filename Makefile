all : toplevel.native test.o prebuilt.o

tests: toplevel.native prebuilt.o

clean: 
	ocamlbuild -clean
	rm -rf parser.mli parser.output parser.ml ocamlllvm *.diff *.o *.output *.ll *.s *.exe *.err *.out *.log tests/*.s tests/*.ll


toplevel.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind toplevel.native

test : test.c
	cc -o test -DBUILD_TEST test.c

prebuilt : prebuilt.c
	cc -o test -DBUILD_TEST prebuilt.c


