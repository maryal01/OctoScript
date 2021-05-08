type action = Ast | Sast | LLVM_IR | Compile

let () =
  let action = ref Compile in
  let set_action a () = action := a in
  let speclist =
    [
      ("-a", Arg.Unit (set_action Ast), "Print the AST");
      ("-s", Arg.Unit (set_action Sast), "Print the SAST");
      ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
      ( "-c",
        Arg.Unit (set_action Compile),
        "Check and print the generated LLVM IR (default)" );
    ]
  in
  let usage_msg = "usage: ./toplevel.native [-a|-s|-l|-c] [file1.oc]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lib_decls, lib_stmtss = Parser.program Scanner.token (Lexing.from_channel (open_in "stdlib.oc")) in

  let lexbuf = Lexing.from_channel !channel in
  let prog_decls, prog_stmts = Parser.program Scanner.token lexbuf in
  let ast = (lib_decls @ prog_decls, lib_stmtss @ prog_stmts) in
  (match !action with
      Ast -> raise (Failure "action not implemented")
    | _   -> let sast = Semant.check ast in
                (match !action with
                    Ast     -> ()
                  | Sast    -> raise (Failure "action not implemented")
                  | LLVM_IR -> raise (Failure "action not implemented")
                  | Compile -> 
                      let m = Codegen.translate sast in
                        Llvm_analysis.assert_valid_module m;
                        print_string (Llvm.string_of_llmodule m)))
