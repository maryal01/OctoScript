all : toplevel.native

toplevel.native : toplevel.ml ast.ml parser.mly scanner.mll
	ocamlbuild toplevel.native
