type boolOp = AND | OR
type binaryOp = Add | Sub | Mul | Div | Pow | Log
type unaryOp = NOT | NEG
type compOp  = GT | GTE | LT | LTE | EQ | NEQ
type dtype = INT | FLOAT | STRING | TABLE | LIST | TUPLE | LAMBDA
(* the binary values of true and false are 1 and 0 respectively *)

type expr = 
	|Binop of expr * binaryOp * expr
	|Boolop of expr * boolOp * expr
	|Unop of unaryOp * expr 
	|Comop of expr * copmOp * expr
	|Lit of int
	|Var of string
	|DataStruct of string (* calling a separate type *)
	|Lambda of expr * expr (* function name, datastruct of string AS arguments, dtype AS returntype *)
	|Apply of string * string * expr (* data structure name, datastructure's function name,  datastruct of string AS arguments,*)
	|Func of string * expr (* *dtype?? if require strict type checking *)

type statement =
	|While  of expr * statement
	|If of expr * statement * statement
	|Return of expr
	|Break
	|Assign of string * expr
	|Print  of string
	|Expr
	|Function of string * expr * expr * dtype

type datastruct = 
    |Table of string * ??
	|Tuple of string * ??
	|List  of string * ??


(*
Lambda: (a, b) -> {expr}
arguments
expression as body

Functions:
name of function
arguments
expression as body
return type


Function Call:
name of function
arguments

Expressions:
Funcall (e.g. function())
*)