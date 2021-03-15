type token =
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | COMMA
  | SEMI
  | DOT
  | COLON
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | POW
  | LOG
  | MOD
  | ASSIGN
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | AND
  | OR
  | FARROW
  | LARROW
  | RETURN
  | IF
  | ELSE
  | WHILE
  | PRINT
  | BREAK
  | INT
  | BOOL
  | FLOAT
  | NONE
  | STRING
  | LAMBDA
  | TABLE
  | LIST
  | TUPLE
  | INPUT
  | OUTPUT
  | ACCESS
  | APPEND
  | LENGTH
  | ILIT of (int)
  | BLIT of (bool)
  | SLIT of (string)
  | ID of (string)
  | FLIT of (float)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
