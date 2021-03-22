open Ast
open Sast

module StringMap = Map.Make(String)

let check (functions, statements) = 
	let type_of_identifier = (* TODO *)
	let find_func = (* TODO *)
	let check_call = (* TODO *)
	let check_assign = (* TODO *)
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
			| _ -> raise (Failure ("illegal binary operator " ^ string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^ string_of_typ t2 ^ " in " ^ string_of_expr e))
			in (ty, SBinop((t1, e1'), op, (t2, e2')))
		| Lambda(args, e) as lambda -> let (t1, e1) = (expr e) in (t1, SLambda(args, (t1, e1)))
		| ListLit(elements) as  list -> 
			[] -> (NONE, elements)
			| elem :: elements -> 
				let (t1, e1) = (expr elem) in
				let all_func elem' ->  
					let (t', e')  = (expr elem') in
					(t1 = t')
				in match List.for_all all_func elements with
				true -> (t1, elements)
				|false -> raise (Failure ("illegal List literal " ^ string_of_typ t1 ^ " expected " ^ " in " ^ string_of_expr e))


		| TupleLit ( elements ) as tuple -> 
			let fold_func elem, acc = let (t1, e1) = (expr elem) in t1 :: acc
			(TUPLE, (List.fold_right fold_func [] elements, elements)) (* TODO: check is sast.ml can be better? *)
		
		|Apply (e, name, expr_list) ->
		|Call(fname, args) as call -> 
		|IfExpr ( e1, e2, e3 ) as e ->	
			let (t1, e1') = expr e1
			and (t2, e2') = expr e2
			and (t3, e3') = expr e3 in
			match (t1, (t2 = t3)) with
				(BOOLEAN, true) -> _
			|_ -> raise (Failure ("illegal if expression " ^ string_of_typ t1 ^ " " ^ string_of_typ t2 ^ " " ^ " " ^ string_of_typ t3 ^ " in " ^ string_of_expr e))
			in
			(t2, SIfExpr((t1,e1'), (t2,e2'), (t3,e3')))


			

	in

(*
1. type checking
2. builds ssymbol table and haandles the variables
*)