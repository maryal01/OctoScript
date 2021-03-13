open Ast

(* Pretty much the same thing from assignment 1
   Prints out parsed tokens for manual checking *)

(* printing needs to be improved, also run through Parser.program
   we need another ast to string function to print out the insides *)

(* lambdas are removed so things compile for once *)

let token_to_string t = 
   match t with 
     Parser.LPAREN -> "LPAREN" 
   | Parser.RPAREN -> "RPAREN" 
   | Parser.LBRACE -> "LBRACE" 
   | Parser.RBRACE -> "RBRACE" 
   | Parser.LBRACK -> "LBRACK" 
   | Parser.RBRACK -> "RBRACK" 
   | Parser.COMMA -> "COMMA" 
   | Parser.SEMI -> "SEMI" 
   | Parser.DOT -> "DOT" 
   | Parser.PLUS -> "PLUS" 
   | Parser.MINUS -> "MINUS" 
   | Parser.TIMES -> "TIMES" 
   | Parser.DIVIDE -> "DIVIDE" 
   | Parser.POW -> "POW" 
   | Parser.LOG -> "LOG" 
   | Parser.ASSIGN -> "ASSIGN" 
   | Parser.NOT -> "NOT" 
   | Parser.EQ -> "EQ" 
   | Parser.NEQ -> "NEQ" 
   | Parser.LT -> "LT" 
   | Parser.LEQ -> "LEQ" 
   | Parser.GT -> "GT" 
   | Parser.GEQ -> "GEQ" 
   | Parser.AND -> "AND" 
   | Parser.OR -> "OR" 
   | Parser.FARROW -> "FARROW" 
   | Parser.LARROW -> "LARROW" 
   | Parser.RETURN -> "RETURN" 
   | Parser.IF -> "IF" 
   | Parser.ELSE -> "ELSE" 
   | Parser.WHILE -> "WHILE" 
   | Parser.PRINT -> "PRINT" 
   | Parser.BREAK -> "BREAK" 
   | Parser.INT -> "INT" 
   | Parser.BOOL -> "BOOL" 
   | Parser.FLOAT -> "FLOAT"
   | Parser.NONE -> "NONE"
   | Parser.STRING -> "STRING"
   | Parser.TABLE -> "TABLE"
   | Parser.LIST -> "LIST"
   | Parser.LAMBDA -> "LAMBDA"
   | Parser.TUPLE -> "TUPLE"
   | Parser.LITERAL i -> "LITERAL " ^ (string_of_int i)
   | Parser.BLIT b -> "BLIT " ^ (string_of_bool b)
   | Parser.ID s -> "ID " ^ s
   | Parser.FLIT f -> "FLIT " ^ (string_of_float f)
   | Parser.STRINGLIT s -> "STRINGLIT" ^ s
   | Parser.EOF -> "EOF"
   
let _ =
  let lex_buf = Lexing.from_channel stdin in
  let expr = Parser.program Scanner.token lex_buf in
  expr