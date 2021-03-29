open Ast
open Sast
module StringMap = Map.Make(String)

let check (functions, statements) = 
	let check_binds (kind : string) (to_check : bind list) = 
		let check_it checked binding = 
			let void_err = "illegal void " ^ kind ^ " " ^ snd binding
			and dup_err = "duplicate " ^ kind ^ " " ^ snd binding
			in match binding with
				(NONE, _) -> raise (Failure void_err)
				|(_, n1) -> match checked with ((_, n2) :: _) when n1 = n2 -> raise (Failure dup_err)
				| _ -> binding :: checked
    	in 
		let _ = List.fold_left check_it [] (List.sort compare to_check) 
    	in to_check
	in 
	let built_in_decls =
		let add_bind map (name, ty) = 
			StringMap.add name {
				typ = NONE; fname = name; 
				formals = [(ty, "x")]; body = [] } map
		in 
		List.fold_left add_bind 
			StringMap.empty [ ("print", INT); ("printb", BOOLEAN); ("printf", FLOAT); ("printbig", INT) ] 
	in
	let add_func map fd = 
		let built_in_err = "function " ^ fd.fname ^ " may not be defined"
		and dup_err = "duplicate function " ^ fd.fname
		and make_err er = raise (Failure er)
		and n = fd.fname 
		in match fd with
			_ when StringMap.mem n built_in_decls -> make_err built_in_err
			| _ when StringMap.mem n map -> make_err dup_err  
			| _ ->  StringMap.add n fd map 
	in
	let check_assign lvaluet rvaluet err =
		if lvaluet = rvaluet then lvaluet else raise (Failure err)
	in
	let function_decls = List.fold_left add_func built_in_decls functions
	in
	let find_func s = 
		try StringMap.find s function_decls
		with Not_found -> raise (Failure ("unrecognized function " ^ s))
	in
	let rec expr = function 
		PrimLit  l -> match l with
			Int i -> (INT , SIntLit i)
			|Float f -> (FLOAT, SFloatLit f)
			|String s -> (STRING, SStringLit s)
			|Boolean b -> (BOOLEAN, SBoolLit b)
		|Noexpr     -> (NONE, SNoExp)
		|Var s       -> (type_of_identifier s, SId s)
		|Unop(op, e) as ex -> 
			let (t, e') = expr e
			in 
			let ty = match op with
				NEG when t = INT || t = FLOAT -> t
				| NOT when t = BOOLEAN -> BOOLEAN
				| _ -> raise (Failure ("illegal unary operator " ^  string_of_uop op ^ string_of_typ t ^ " in " ^ string_of_expr ex))
			in (ty, SUnop(op, (t, e')))
		|Binop(e1, op, e2) as e -> 
			let (t1, e1') = expr e1 
			and (t2, e2') = expr e2 
			in
			let same = t1 = t2 
			in
			let ty = match op with
				Add | Sub | Mult | Div when same && t1 = INT   -> INT
				| Add | Sub | Mult | Div when same && t1 = FLOAT -> FLOAT 
				| EQ | NEQ            when same               -> BOOLEAN
				| LTE | LT | GT | GEQ when same && (t1 = INT || t1 = FLOAT) -> BOOLEAN
				| AND | OR when same && t1 = BOOLEAN -> BOOLEAN
				| Pow when same && t1 = INT -> INT 
				| Log when same && t1 = INT -> FLOAT
				| Mod when same && t1 = INT -> INT
				| _ -> raise (Failure ("illegal binary operator " ^ string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^ string_of_typ t2 ^ " in " ^ string_of_expr e))
			in (ty, SBinop((t1, e1'), op, (t2, e2')))
		| Lambda(args, e) as lambda -> let (t1, e1) = (expr e) in (t1, SLambda(args, (t1, e1)))
		| ListLit(elements) as  list -> match list with
			[] -> (NONE, elements)
			| elem :: elements -> 
				let (t1, e1) = (expr elem) 
				in
				let all_func elem' =  let (t', e')  = (expr elem') in (t1 = t')
				in 
				match List.for_all all_func elements with
					true -> (t1, elements)
					|false -> raise (Failure ("illegal List literal " ^ string_of_typ t1 ^ " expected " ^ " in " ^ string_of_expr e))
		| TupleLit ( elements ) as tuple -> 
			let fold_func acc elem = 
				let (t1, e1) = (expr elem) 
				in t1 :: acc
			in
			(TUPLE, (List.fold_right fold_func [] elements, elements))
		|Apply (e, name, expr_list) -> SApply(expr e, name, List.map expr expr_list)(* TODO *)
		|Call(fname, args) as call ->  SList(fname, List.map expr args)  (* TODO *)
		|IfExpr ( e1, e2, e3 ) as e ->	
			let (t1, e1') = expr e1
			and (t2, e2') = expr e2
			and (t3, e3') = expr e3 
			in
			match (t1, (t2 = t3)) with
				(BOOLEAN, true) -> (t2, SIfExpr((t1,e1'), (t2,e2'), (t3,e3')))
				|_ -> raise (Failure ("illegal if expression " ^ string_of_typ t1 ^ " " ^ string_of_typ t2 ^ " " ^ " " ^ string_of_typ t3 ^ " in " ^ string_of_expr e))			
	in     
	let check_bool_expr e = 
		let (t', e') = expr e
		and err = "expected Boolean expression in " ^ string_of_expr e
	in if t' != Bool then raise (Failure err) else (t', e') 
	in
	let rec check_stmt = function
		Expr e -> SExpr (expr e)
		| If(p, b1, b2) -> 
			let p' = check_bool_expr p and 
				b1' = List.map check_stmt b1 and 
				b2' = List.map check_stmt b2 in
			SIf(p', b1', b2')
		| While(p, s) -> 
			let p' = check_bool_expr p and
				s' = List.map check_stmt s in
			SWhile(p', s')
		| Return e ->  let (t, e') = expr e in
			if t = func.typ then SReturn (t, e') 
			else raise (
					Failure ("Returning " ^ string_of_typ t ^ " while expected is " ^
									string_of_typ func.typ ^ " in expression " ^ 
								string_of_expr e))
		| Assign (s, e) -> 
			let lt = type_of_identifier s (* TODO :: REPLACE *)
			and (rt, e') = expr e in 
			if rt = lt  then SAssign(s, (rt, e'))
			else raise (
					Failure ("Illegal assignment of " ^ string_of_typ lt ^ 
								" and " ^ string_of_typ rt ^ " in " ^
								string_of_expr (Assign(s, e))))
		| Print (e) -> let e' = expr e in 
			match e' with 
			  (_, SPrimLit) -> e'
			| _             -> raise 
								( Failure ("Can only print StringLiterals ." ^ 
											" Problem in" ^ string_of_expr e))
		| Break -> SBreak
	in 
	(List.map check_stmt statements, List.map check_function functions)