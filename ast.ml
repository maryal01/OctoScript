type Op = AND | OR | Add | Sub | Mul | Div | Pow | Log | GT | GTE | LT | LTE | EQ | NEQ
type unaryOp = NOT | NEG

type prim = Int of int | String of string| Float of float | Boolean of bool
type dtype = INT | FLOAT | STRING | BOOLEAN | LAMBDA | NONE | TABLE | TUPLE | LIST
type datastruct = TABLE | TUPLE | LIST 
type bind = prim * string

type func_decl = {
	typ : dtype;
    	fname : string;
    	formals : bind list;
    	body : stmt list;
    	types: prim list option; (* what is this? *)
  }

type expr = 
	|Binop of expr * binaryOp * expr
	|Unop of unaryOp * expr 
	|PrimLit of prim
	|ListLit of prim list
	|Var of string
	|Datastruct of datastruct (* this is what im talking. should it be this or Datastruct of string ?? *)
	|Lambda of func_decl
	|Apply of expr * string * expr list
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


  
 type program = bind list * func_decl list
