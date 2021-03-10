type Op = AND | OR | Add | Sub | Mul | Div | Pow | Log | GT | GTE | LT | LTE | EQ | NEQ
type unaryOp = NOT | NEG

type prim = Int of int | String of string| Float of float | Boolean of bool
type dtype = INT | FLOAT | STRING | BOOLEAN | LAMBDA | NONE | TABLE | TUPLE | LIST

type bind = prim * string
type func =  bind list * statement list * dtype (* why have func when there is func_decl? *) (* why is there bind list there? *)

type datastruct = {
	typ: TABLE | TUPLE | LIST;
	name: string;
}

type expr = 
	|Binop of expr * binaryOp * expr
	|Unop of unaryOp * expr 
	|PrimLit of prim
	|ListLit of prim list
	|Var of string
	|Lambda of func
	|Apply of datastruct * func
	|FuncCall of string * expr list
	|Noexpr
 
type statement =
	|While  of expr * statement list
	|If of expr * statement * statement
	|Return of expr
	|Break
	|Assign of string * expr
	|Print  of string
	|Expr of expr

type func_decl = {
	typ : dtype;
    	fname : string;
    	formals : bind list;
    	body : stmt list;
    	types: prim list option; (* what is this? *)
  }
  
 type program = bind list * func_decl list
