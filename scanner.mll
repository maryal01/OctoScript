{ open Parser }

let digit = ['0' - '9']
let digits = digit+

(* dot before inbuilt function *)
(* quotations to recognize string *)
(* table, list and tuple type *)
(* differentiate recognizing minus and gt vs farrow *)
(* added farrow and larrow for functions and lambdas respectively *)

(* recognizing primitive functions -- read, sort, join, filter, update, more *)
rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"      { comment lexbuf }           (* Comments *)
| '\"'      { strvalue lexbuf}

| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACK }
| ']'      { RBRACK }

| ';'      { SEMI }
| ','      { COMMA }
| '.'      { DOT }

| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '^'      { POW    }
| "log"    { LOG    }
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
| "print" { PRINT }

| "int"    { INT }
| "bool"   { BOOL }
| "float"  { FLOAT }
| "string" { STRING }

| "None"   { NONE }

| "Table"  { TABLE }
| "List"   { LIST }
| "Tuple"  { TUPLE }

| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }

| digits as lxm { LITERAL(int_of_string lxm) }
| digits '.'  digit* as lxm { FLIT(float_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
    "*/" { token lexbuf }
    | _    { comment lexbuf }

and strvalue = parse
    "\""  { token lexbuf }
    | _ { strvalue lexbuf }
