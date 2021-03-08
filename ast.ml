type boolOp = AND | OR
type binaryOp = Add | Sub | Mul | Div | Pow | Log
type unaryOp = NOT | NEG
type compOp  = GT | GTE | LT | LTE | EQ | NEQ
type dtype = INT | FLOAT | STRING | BOOLEAN | LAMBDA | NONE
type data = TABLE | TUPLE | LIST
type func =  string list * statement list 
(* the binary values of true and false are 1 and 0 respectively *)

type prim = 
    | Int of int
    | String of string
    | Float of float
    | Boolean of bool


type expr = 
	|Binop of expr * binaryOp * expr
	|Boolop of expr * boolOp * expr
	|Unop of unaryOp * expr 
	|Comop of expr * copmOp * expr
	|PrimLit of prim
	|DataStruct of data
	|ListLit of prim list
	|Var of string
	|Lambda of func (*|| parameters * body ||*)
	|Apply of expr * func  (*|| (DataStruct of data) * func   ||*)
	|FuncCall of func * expr list (*|| func_name * parameters ||*)
 
type statement =
	|While  of expr * statement
	|If of expr * statement * statement
	|Return of expr
	|Break
	|Assign of string * expr
	|Print  of string
	|Expr of expr
	|Function of string * func * dtype
