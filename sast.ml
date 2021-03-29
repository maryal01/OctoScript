open Ast

type sexpr = typ * sx

and sx =
  | SIntLit of int
  | SFloatLit of float
  | SStringLit of string
  | SBoolLit of bool
  | SListLit of typ * prim list
  | STupleLit of typ list * prim list
  | STableLit of typ list * prim list list
  | SBinop of sexpr * binaryOp * sexpr
  | SUnop of unaryOp * sexpr
  | SIfExpr of sexpr * sexpr * sexpr
  | SLambda of bind list * sexpr
  | SVar of string
  | SApply of sexpr * string * sexpr list
  | SCall of string * sexpr list
  | SNoExp

type sstmt =
  | SWhile of sexpr * stmnt list
  | SIf of sexpr * sstmt list * sstmt list
  | SReturn of sexpr
  | SBreak
  | SDeclare of typ * string * sexpr
  | SAssign of string * sexpr
  | SPrint of sexpr
  | SExpr of sexpr

type sfunc_decl = {
  styp : typ;
  sfname : string;
  sformals : bind list;
  sbody : sstmt list;
}

type sprogram = sfunc_decl list * sstmt list

let string_of_sprogram = " "