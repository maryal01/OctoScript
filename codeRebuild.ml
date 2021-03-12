open Ast

(* Pretty much the same thing from assignment 1
   Prints out parsed tokens for manual checking *)

(* printing needs to be improved, also run through Parser.program
   we need another ast to string function to print out the insides *)

(* lambdas are removed so things compile for once *)

let biop_to_string o = 
    match o with
      Ast.AND -> "&"  
    | Ast.OR  -> "|"  
    | Ast.Add -> "+"  
    | Ast.Sub -> "-"  
    | Ast.Mul -> "*"  
    | Ast.Div -> "/"  
    | Ast.Pow -> "^"  
    | Ast.Log -> "log"  
    | Ast.GT  -> ">"  
    | Ast.GTE -> ">="  
    | Ast.LT  -> "<"  
    | Ast.LTE -> "<="  
    | Ast.EQ  -> "=="  
    | Ast.NEQ -> "!=" 

let unop_to_string o =
    match o with Ast.NOT -> "!" | Ast.NEG -> "-"

let prim_to_string p =
    match p with
      Ast.Int i -> string_of_int i 
    | Ast.String s -> "\"" ^ s ^ "\""
    | Ast.Float f -> string_of_float f
    | Ast.Boolean b -> string_of_bool b

let rtype_to_string t =
    match t with
      Ast.INT       -> "int"
    | Ast.FLOAT     -> "float"
    | Ast.STRING    -> "string"
    | Ast.BOOLEAN   -> "boolean"
    | Ast.LAMBDA    -> "lambda"
    | Ast.NONE      -> "void"
    | Ast.TABLE     -> "table"
    | Ast.TUPLE     -> "tuple"
    | Ast.LIST      -> "list"

let bind_to_string (r, s) = (rtype_to_string r) ^ " " ^ s

let rec expr_to_string e =
    let rec plist_to_string pl =
        match pl with
         [] -> ""
        | (p :: []) -> prim_to_string p
        | (p :: ps) -> (prim_to_string p) ^ ", " ^ (plist_to_string ps)
    in let rec elist_to_string el =
        match el with
         [] -> ""
        | (e :: []) -> expr_to_string e
        | (e :: es) -> (expr_to_string e) ^ ", " ^ (elist_to_string es)
    in
    match e with
      Ast.Binop (e1, bop, e2) -> 
        expr_to_string e1 ^ " " ^ biop_to_string bop ^ " " ^ expr_to_string e2 
	| Ast.Unop (uop, e) -> unop_to_string uop ^ expr_to_string e
	| Ast.PrimLit p -> prim_to_string p
	| Ast.ListLit ps -> "[" ^ (plist_to_string ps) ^ "]"
	| Ast.TupleLit ps -> "(" ^ (plist_to_string ps) ^ ")"
	| Ast.Var s -> s
	| Ast.Apply (e, s, es) -> (expr_to_string e) ^ "." ^ s ^ "(" ^ (elist_to_string es) ^ ")"
	| Ast.Call (s, es) -> s ^ "(" ^ (elist_to_string es) ^ ")"
	| Ast.Noexpr -> "NO_EXP"

let rec stmt_to_string s = 
    let rec slist_to_string sl = 
        match sl with
          [] -> ""
        | (s :: []) -> stmt_to_string s ^ ";"
        | (s :: sl) -> (stmt_to_string s ^ ";\n") ^ (slist_to_string sl)
    in
    match s with
      Ast.While (e, sl) -> 
        "while(" ^ expr_to_string e ^ "){\n" ^ slist_to_string sl ^ "\n}"
	| Ast.If (e, sl1, sl2) -> 
        "if(" ^ expr_to_string e ^ "){\n" ^ slist_to_string sl1  ^ "\n}else{\n" ^ slist_to_string sl2 ^ "\n}"
	| Ast.Return e -> "return"
	| Ast.Break -> "break"
	| Ast.Assign (s, e) -> s ^ " = " ^ expr_to_string e 
	| Ast.Print e -> "print(" ^ expr_to_string e ^ ")"
	| Ast.Expr e -> expr_to_string e
	| Ast.FunDecl (s, bs, rt, sl) -> 
        let rec bs_to_string bs = 
            match bs with
              [] -> ""
            | (b :: []) -> bind_to_string b
            | (b :: bs) ->  bind_to_string b ^ ", " ^ bs_to_string bs
        in s ^ "(" ^ bs_to_string bs ^ ") -> " ^ rtype_to_string rt ^ "{\n" ^
             slist_to_string sl ^ "\n}"

let rec prog_to_string p = 
    match p with
      [] -> ""
    | (s :: sl) -> stmt_to_string s ^ ";\n" ^ prog_to_string sl

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
   
let () =
  let lex_buf = Lexing.from_channel stdin in
  let prog = Parser.program Scanner.token lex_buf in
  print_string (prog_to_string prog)
