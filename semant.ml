open Ast
open Sast

module StringMap = Map.Make(String)

let check (functions, statements) = 
	let type_of_identifier = (* TODO *)
	
	let rec expr = function
        PrimLit  l -> 
			match l with
			Int i -> (INT , SPrimLit l)
			|Float f -> (FLOAT, SPrimLit l)
			|String s -> (STRING, SPrimLit l)
			|Boolean b -> (BOOLEAN, SPrimLit l)
      	|Noexpr     -> (NONE, SNoExp)
      	|Var s       -> (type_of_identifier s, SId s)
      	|Unop(op, e) as ex -> 
          let (t, e') = expr e in
          let ty = match op with
            NEG when t = INT || t = FLOAT -> t
          | NOT when t = BOOLEAN -> BOOLEAN
          | _ -> raise (Failure ("illegal unary operator " ^  string_of_uop op ^ string_of_typ t ^ " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))
      	|Binop(e1, op, e2) as e -> 
          let (t1, e1') = expr e1 
          and (t2, e2') = expr e2 in
          let same = t1 = t2 in
          let ty = match op with
            Add | Sub | Mult | Div when same && t1 = INT   -> INT
          | Add | Sub | Mult | Div when same && t1 = FLOAT -> FLOAT (* TODO: remove if needed ? *)
          | EQ | NEQ            when same               -> BOOLEAN
          | LTE | LT | GT | GEQ when same && (t1 = INT || t1 = FLOAT) -> BOOLEAN
          | AND | OR when same && t1 = BOOLEAN -> BOOLEAN
		  | Pow when same && t1 = INT -> INT (* TODO can't handle large power operations *)
		  | Log when same && t1 = INT -> FLOAT
		  | Mod when same && t1 = INT -> INT
          | _ -> raise (
	      Failure ("illegal binary operator " ^ string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^ string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      	|Call(fname, args) as call -> 
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e = 
            let (et, e') = expr e in 
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in 
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'))
		| ListLit(prim_list ) -> 
		| TupleLit ( prim_list ) ->
		| Apply (e, name, expr_list) ->
		| IfExpr ( e1, e2, e3 ) ->	
    in

(*
1. type checking
2. builds ssymbol table and haandles the variables
*)