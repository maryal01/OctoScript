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

| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }

| "&"      { AND }
| "|"      { OR }
| "!"      { NOT }

| "->"     { FARROW }
| "=>"     { LARROW }

| "if"     { IF }
| "else"   { ELSE }
| "while"  { WHILE }
| "return" { RETURN }
| "break"  { BREAK }
| "print"  { PRINT }

| "int"    { INT }
| "bool"   { BOOL }
| "float"  { FLOAT }
| "string" { STRING }
| "none"   { NONE }
| "table"  { TABLE }
| "list"   { LIST }
| "tuple"  { TUPLE }

| "read"  { INPUT  }
| "write" { OUTPUT }
| "get"   { ACCESS }
| "append" { APPEND }
| "length" { LENGTH }
| "fn"     { FUNC }

| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| '\"' (string* as str) '\"' { SLIT(str) }
| digits as lxm { ILIT(int_of_string lxm) }
| digits '.'  digit* as lxm { FLIT(float_of_string lxm) }

| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
    "*/" { token lexbuf }
    | _    { comment lexbuf }
