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
  | SCall of string * sexpr list
  | SNoExp

type sstmt =
  | SBlock of sstmt list
  | SWhile of sexpr * sstmt
  | SIf of sexpr * sstmt * sstmt
  | SReturn of sexpr
  | SBreak
  | SDeclare of typ * string * sexpr
  | SAssign of string * sexpr
  | SExpr of sexpr

type sfunc_decl = {
  styp : typ;
  sfname : string;
  sformals : bind list;
  sbody : sstmt list;
}

type sprogram = sfunc_decl list * sstmt list

let rec tylist_to_string = function
  | [] -> ""
  | [ first ] -> typ_to_string first
  | first :: rem -> typ_to_string first ^ ", " ^ tylist_to_string rem

let rec blist_to_string = function
  | [] -> ""
  | [ first ] -> bind_to_string first
  | first :: rem -> bind_to_string first ^ ", " ^ blist_to_string rem

let rec plist_to_string = function
  | [] -> ""
  | [ first ] -> prim_to_string first
  | first :: rem -> prim_to_string first ^ ", " ^ plist_to_string rem

let rec sexpr_to_string (ty, sx) =
  let rec sexprlist_to_string = function
    | [] -> ""
    | [ first ] -> sexpr_to_string first
    | first :: rem -> sexpr_to_string first ^ ", " ^ sexprlist_to_string rem
  in
  let sx_to_string sx =
    (* let rec sxlist_to_string = function
      | [] -> ""
      | [ first ] -> sx_to_string first
      | first :: rem -> sx_to_string first ^ ", " ^ sxlist_to_string rem
    in *)
    match sx with
    | SIntLit i -> string_of_int i
    | SFloatLit f -> string_of_float f
    | SStringLit s -> s
    | SBoolLit b -> string_of_bool b
    | SListLit (ty, pl) ->
        "SListLit: \n Type: " ^ typ_to_string ty ^ "\n List: "
        ^ plist_to_string pl
    | STupleLit (tyl, pl) ->
        "STupleLit: \n Types: " ^ tylist_to_string tyl ^ "\n List: "
        ^ plist_to_string pl
    | STableLit (tyl, pll) ->
        let rec table_to_string ps =
          match ps with
          | [] -> ""
          | [ p ] -> "[" ^ plist_to_string p ^ "]\n"
          | p :: ps -> "[" ^ plist_to_string p ^ "]\n" ^ table_to_string ps
        in
        "STableLit: \n Types: " ^ tylist_to_string tyl ^ table_to_string pll
    | SBinop (sx1, binop, sx2) ->
        sexpr_to_string sx1 ^ biop_to_string binop ^ sexpr_to_string sx2
    | SUnop (uop, sx) -> unop_to_string uop ^ sexpr_to_string sx
    | SIfExpr (sx1, sx2, sx3) ->
        "if( " ^ sexpr_to_string sx1 ^ "\nthen { " ^ sexpr_to_string sx2
        ^ "}\n else {" ^ sexpr_to_string sx3 ^ "}\n"
    | SLambda (bl, sx) ->
        "(" ^ blist_to_string bl ^ ") => " ^ "{ " ^ sexpr_to_string sx ^ " }"
    | SVar s -> "Variable " ^ s
    | SCall (s, sxl) -> s ^ "(" ^ sexprlist_to_string sxl ^ ")"
    | SNoExp -> "SNoExpr"
  in
  "Sexpr( " ^ typ_to_string ty ^ ", " ^ sx_to_string sx ^ " )"

let rec sslist_to_string sl =
  let rec sstmt_to_string = function
    | SBlock stl -> "Block { " ^ sslist_to_string stl  ^ " }"
    | SWhile (sx, st) ->
        "while( " ^ sexpr_to_string sx ^ ")\n{" ^ sstmt_to_string st ^ "}\n"
    | SIf (sx, st1, st2) ->
        "if( " ^ sexpr_to_string sx ^ "\nthen { " ^ sstmt_to_string st1
        ^ "}\n else {" ^ sstmt_to_string st2 ^ "}\n"
    | SReturn sx -> "Sreturn " ^ sexpr_to_string sx
    | SBreak -> "SBreak"
    | SDeclare (ty, str, sx) ->
        "Sdeclare: " ^ typ_to_string ty ^ " " ^ str ^ " = " ^ sexpr_to_string sx
    | SAssign (str, sx) -> "SAssign: " ^ str ^ " = " ^ sexpr_to_string sx
    | SExpr sx -> "SExpr " ^ sexpr_to_string sx
  in
  match sl with
  | [] -> ""
  | first :: rem -> sstmt_to_string first ^ "\n" ^ sslist_to_string rem

let sfdecl_to_string func =
  typ_to_string func.styp ^ " " ^ func.sfname ^ "("
  ^ blist_to_string func.sformals
  ^ ")\n{"
  ^ sslist_to_string func.sbody
  ^ " }"

let  sprog_to_string (sfl, ssl) =
  let rec sfdecllist_to_string = function
    | [] -> ""
    | first :: rem -> sfdecl_to_string first ^ "\n" ^ sfdecllist_to_string rem
  in
  "Functions: \n" ^ sfdecllist_to_string sfl ^ "Statements: \n"
  ^ sslist_to_string ssl
