type binaryOp = AND | OR | Add | Sub | Mul | Div | Pow | Log | GT | GTE | LT | LTE | EQ | NEQ
type unaryOp = NOT | NEG
type prim = Int of int | String of string| Float of float | Boolean of bool
type rtype = INT | FLOAT | STRING | BOOLEAN | LAMBDA | NONE | TABLE | TUPLE | LIST
type bind = prim * string

type expr = 
	|Binop of expr * binaryOp * expr
	|Unop of unaryOp * expr 
	|PrimLit of prim
	|ListLit of prim list
	|TupleLit of prim list
	(* |Lambda of func_decl *)
	|Var of string
	|Apply of expr * string * expr list
	|Call of string * expr list
	|Noexpr
 
type stmnt =
	|While  of expr * stmnt list
	|If of expr * stmnt * stmnt
	|Return of expr
	|Break
	|Assign of string * expr
	|Print  of string
	|Expr of expr

type func_decl = {
	    typ : rtype;
    	fname : string;
    	formals : bind list;
	    (* locals: bind list; *)
    	body : stmnt list;
    	types: prim list option; (* what is this? *) 
      }


type program = bind list * func_decl list