type binaryOp = AND | OR | Add | Sub | Mul | Div | Pow | Log | GT | GTE | LT | LTE | EQ | NEQ
type unaryOp = NOT | NEG
type prim = Int of int | String of string| Float of float | Boolean of bool
type rtype = INT | FLOAT | STRING | BOOLEAN | LAMBDA | NONE | TABLE | TUPLE | LIST
type bind = rtype * string

type expr = 
	|Binop of expr * binaryOp * expr
	|Unop of unaryOp * expr 
	|PrimLit of prim
	|ListLit of prim list
	|TupleLit of prim list
	|Var of string
	|Apply of expr * string * expr list
	|Call of string * expr list
	|Noexpr
 
type stmnt =
	|While  of expr * stmnt list
	|If of expr * stmnt list * stmnt list
	|Return of expr
	|Break
	|Assign of string * expr
	|Print  of expr
	|Expr of expr
	|FunDecl of string * bind list * rtype * stmnt list

(* type func_decl = {
	typ: rtype;
	fname: string;
	formals: bind list;
	body: stmnt list;
} *)
(*  need to keep func_decl separate from the statement list because don't want functions to be defined inside functions *)
type program =  stmnt list 


(***********************************************************************************************************************)
(* Pretty-printing functions *)
let biop_to_string o = 
	match o with
		AND -> "&"  
	| OR  -> "|"  
	| Add -> "+"  
	| Sub -> "-"  
	| Mul -> "*"  
	| Div -> "/"  
	| Pow -> "^"  
	| Log -> "log"  
	| GT  -> ">"  
	| GTE -> ">="  
	| LT  -> "<"  
	| LTE -> "<="  
	| EQ  -> "=="  
	| NEQ -> "!=" 

let unop_to_string o =
	match o with NOT -> "!" | NEG -> "-"

let prim_to_string p =
	match p with
		Int i -> string_of_int i 
	| String s -> "\"" ^ s ^ "\""
	| Float f -> string_of_float f
	| Boolean b -> string_of_bool b

let rtype_to_string t =
	match t with
		INT       -> "int"
	| FLOAT     -> "float"
	| STRING    -> "string"
	| BOOLEAN   -> "boolean"
	| LAMBDA    -> "lambda"
	| NONE      -> "void"
	| TABLE     -> "table"
	| TUPLE     -> "tuple"
	| LIST      -> "list"

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
		Binop (e1, bop, e2) -> 
			expr_to_string e1 ^ " " ^ biop_to_string bop ^ " " ^ expr_to_string e2 
| Unop (uop, e) -> unop_to_string uop ^ expr_to_string e
| PrimLit p -> prim_to_string p
| ListLit ps -> "[" ^ (plist_to_string ps) ^ "]"
| TupleLit ps -> "(" ^ (plist_to_string ps) ^ ")"
| Var s -> s
| Apply (e, s, es) -> (expr_to_string e) ^ "." ^ s ^ "(" ^ (elist_to_string es) ^ ")"
| Call (s, es) -> s ^ "(" ^ (elist_to_string es) ^ ")"
| Noexpr -> "NO_EXP"

let rec stmt_to_string s = 
	let rec slist_to_string sl = 
			match sl with
				[] -> ""
			| (s :: []) -> stmt_to_string s ^ ";"
			| (s :: sl) -> (stmt_to_string s ^ ";\n") ^ (slist_to_string sl)
	in
	match s with
		While (e, sl) -> 
			"while(" ^ expr_to_string e ^ "){\n" ^ slist_to_string sl ^ "\n}"
| If (e, sl1, sl2) -> 
			"if(" ^ expr_to_string e ^ "){\n" ^ slist_to_string sl1  ^ "\n}else{\n" ^ slist_to_string sl2 ^ "\n}"
| Return e -> "return"
| Break -> "break"
| Assign (s, e) -> s ^ " = " ^ expr_to_string e 
| Print e -> "print(" ^ expr_to_string e ^ ")"
| Expr e -> expr_to_string e
| FunDecl (s, bs, rt, sl) -> 
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