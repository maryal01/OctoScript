{ open Parser }

let digit = ['0' - '9']
let digits = digit+
let string = ([^ '\"'] | "\\\"")
rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"      { comment lexbuf }           (* Comments *)

| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACK }
| ']'      { RBRACK }

| ';'      { SEMI }
| ','      { COMMA }
| ':'      { COLON }
| '.'      { DOT }

| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '^'      { POW   }
| '%'      { MOD   }
| "log"    { LOG   }
| '='      { ASSIGN }

| "=="     { OP_EQ }
| "!="     { OP_NEQ }
| '<'      { OP_LT }
| "<="     { OP_LEQ }
| ">"      { OP_GT }
| ">="     { OP_GEQ }
| "&"      { OP_AND }
| "|"      { OP_OR }
| "!"      { OP_NOT }

| "->"     { FARROW }
| "=>"     { LARROW }

| "if"     { IF }
| "else"   { ELSE }
| "while"  { WHILE }
| "return" { RETURN }
| "break"  { BREAK }

| "int"    { TYP_INT }
| "bool"   { TYP_BOOL }
| "float"  { TYP_FLOAT }
| "string" { TYP_STRING }
| "none"   { TYP_NONE }
| "table"  { TYP_TABLE }
| "list"   { TYP_LIST }
| "tuple"  { TYP_TUPLE }
| "fn"     { FUNC }

| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| '\"' (string* as str) '\"' { SLIT(str) }
| digits as lxm { ILIT(int_of_string lxm) }
| digits '.'  digit* as lxm { FLIT(float_of_string lxm) }

| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_' '-']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
    "*/" { token lexbuf }
    | _    { comment lexbuf }
