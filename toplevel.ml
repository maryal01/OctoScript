open Ast

(* Pretty much the same thing from assignment 1
   Prints out parsed tokens for manual checking *)

let token_to_string t = "token"
   (* match t with 
     LPAREN -> "LPAREN" 
   | RPAREN -> "RPAREN" 
   | LBRACE -> "LBRACE" 
   | RBRACE -> "RBRACE" 
   | LBRACK -> "LBRACK" 
   | RBRACK -> "RBRACK" 
   | COMMA -> "COMMA" 
   | SEMI -> "SEMI" 
   | DOT -> "DOT" 
   | PLUS -> "PLUS" 
   | MINUS -> "MINUS" 
   | TIMES -> "TIMES" 
   | DIVIDE -> "DIVIDE" 
   | POW -> "POW" 
   | LOG -> "LOG" 
   | ASSIGN -> "ASSIGN" 
   | NOT -> "NOT" 
   | EQ -> "EQ" 
   | NEQ -> "NEQ" 
   | LT -> "LT" 
   | LEQ -> "LEQ" 
   | GT -> "GT" 
   | GEQ -> "GEQ" 
   | AND -> "AND" 
   | OR -> "OR" 
   | FARROW -> "FARROW" 
   | LARROW -> "LARROW" 
   | RETURN -> "RETURN" 
   | IF -> "IF" 
   | ELSE -> "ELSE" 
   | WHILE -> "WHILE" 
   | PRINT -> "PRINT" 
   | BREAK -> "BREAK" 
   | INT -> "INT" 
   | BOOL -> "BOOL" 
   | FLOAT -> "FLOAT"
   | VOID -> "VOID"
   | NONE -> "NONE"
   | STRING -> "STRING"
   | TABLE -> "TABLE"
   | LIST -> "LIST"
   | TUPLE -> "TUPLE"
   | LITERAL i -> "LITERAL " @ (string_of_int i)
   | BLIT b -> "BLIT " @ (string_of_bool b)
   | ID s -> "ID " @ s
   | FLIT f -> "FLIT " @ (string_of_float f)
   | EOF -> "EOF" *)
   
let () =
  let lex_buf = Lexing.from_channel stdin in
  let expr = Scanner.token lex_buf in
  (* let expr = Parser.program Scanner.token lex_buf in *)
  print_endline (token_to_string expr)
