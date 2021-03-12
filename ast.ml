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

type program = stmnt list