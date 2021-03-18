
open Ast

let token_to_string t = 
   match t with 
     Parser.LPAREN -> "LPAREN" 
   | Parser.RPAREN -> "RPAREN" 
   | Parser.LBRACE -> "LBRACE" 
   | Parser.RBRACE -> "RBRACE" 
   | Parser.LBRACK -> "LBRACK" 
   | Parser.RBRACK -> "RBRACK" 
   | Parser.COMMA -> "COMMA" 
   | Parser.COLON -> "COLON"
   | Parser.SEMI -> "SEMI" 
   | Parser.DOT -> "DOT" 
   | Parser.PLUS -> "PLUS" 
   | Parser.MINUS -> "MINUS" 
   | Parser.TIMES -> "TIMES" 
   | Parser.MOD -> "MOD"
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
   | Parser.ILIT i -> "LITERAL " ^ (string_of_int i)
   | Parser.BLIT b -> "BLIT " ^ (string_of_bool b)
   | Parser.ID s -> "ID " ^ s
   | Parser.FLIT f -> "FLIT " ^ (string_of_float f)
   | Parser.SLIT s -> "STRINGLIT" ^ s
   | Parser.EOF -> "EOF"
   | Parser.INPUT -> "INPUT"
   | Parser.OUTPUT -> "OUTPUT"
   | Parser.ACCESS -> "ACCESS"
   | Parser.APPEND -> "APPEND"
   | Parser.LENGTH -> "LENGTH"
   | Parser.FUNC -> "fn"
   
let () =
  let lex_buf = Lexing.from_channel stdin in
  let prog = Parser.program Scanner.token lex_buf in
  print_string (Ast.prog_to_string prog)