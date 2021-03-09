open Ast

(* Pretty much the same thing from assignment 1
   Prints out parsed tokens for manual checking *)

let () =
  let lex_buf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.tokenize lex_buf in
  print_endline expr
