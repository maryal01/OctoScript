all : toplevel.native codeRebuild.native

clean: 
	ocamlbuild -clean
	rm -rf parser.mli parser.output parser.ml ocamlllvm *.diff

toplevel.native : toplevel.ml ast.ml parser.mly scanner.mll
	ocamlbuild toplevel.native

codeRebuild.native: codeRebuild.ml ast.ml parser.mly scanner.mll
	ocamlbuild codeRebuild.native