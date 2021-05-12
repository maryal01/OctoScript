open Ast
open Sast
module StringMap = Map.Make (String)
module P = Predef

type symbol_table = {
  identifiers : typ StringMap.t;
  parent : symbol_table option;
}

let check (functions, statements) =
  let check_binds (to_check : bind list) =
    let check_it checked binding =
      let void_err = "illegal void " ^ snd binding
      and dup_err = "duplicate " ^ snd binding in
      match binding with
      | NONE, _ -> raise (Failure void_err)
      | _, n1 -> (
          match checked with
          | (_, n2) :: _ when n1 = n2 -> raise (Failure dup_err)
          | _ -> binding :: checked)
    in
    let _ = List.fold_left check_it [] (List.sort compare to_check) in
    to_check
  in
  let built_in_decls =
    let add_bind map (name, _, ty, ps) =
      let formal_types = function P.Fixed ts -> ts | P.Var ts -> ts in
      let is_var = function P.Fixed _ -> false | P.Var _ -> true in
      StringMap.add name
        {
          typ = ty;
          fname = name;
          formals = List.map (fun t -> (t, "p")) (formal_types ps);
          body = [];
          is_vararg = is_var ps;
          is_overload = false; 
        }
        map
    in
    List.fold_left add_bind StringMap.empty P.predefs
  in
  let overload_name =
    let counter = ref 0 in
    let next_id () =
      counter := !counter + 1;
      !counter
    in fun () -> "_overload_" ^ string_of_int (next_id ()) 
  in
  (* overload_map maps overloaded name to original name *)
  let add_func (map, overload_map) fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined" in
    let is_overload = fd.is_overload in
    let n = fd.fname in
    let add_ov_map_entry _ =
      let ov_name = fd.fname ^ (overload_name ()) in
      let ov_decl = { typ=fd.typ; fname=ov_name; formals=fd.formals; body=fd.body; is_vararg=fd.is_vararg; is_overload=fd.is_overload;} in
      if StringMap.mem n overload_map
      then 
        (* TODO: All this code just to check for duplicate types? 😞😞 *)
        let existing_decls = StringMap.find n overload_map in
        let current_argtype = List.map (fun (t, _) -> t) ov_decl.formals in
        let existing_argtypes = List.map (fun d -> (List.map (fun (t, _) -> t) d.formals)) existing_decls in
        let _ = List.map (fun x -> if x = current_argtype then (raise (Failure ("Overloaded function " ^ n ^ " with same signature declared twice"))) else ()) existing_argtypes
        in StringMap.add n (ov_decl :: existing_decls) overload_map
      else StringMap.add n (ov_decl :: []) overload_map
    in
    let make_err er = raise (Failure er)
    and dup_err = "duplicate function " ^ n in
    if is_overload then (map, add_ov_map_entry ())
    else
    match fd with
      | _ when StringMap.mem n built_in_decls -> make_err built_in_err
      | _ when StringMap.mem n map -> make_err dup_err
      | _ -> (StringMap.add n fd map, overload_map)
  in
  let check_assign lvaluet rvaluet =
    let type_is_generic = 
      (match (rvaluet, lvaluet) with
        | (LIST _, LIST None)  -> Some rvaluet
        | (TUPLE _, TUPLE None) -> Some rvaluet
        | (TABLE _, TABLE None) -> Some rvaluet
        | (LIST None, LIST _)  -> Some lvaluet
        | (TUPLE None, TUPLE _) -> Some lvaluet
        | (TABLE None, TABLE _) -> Some lvaluet
        | _ -> None)
    in
    if lvaluet = rvaluet then rvaluet
    else 
      if Option.is_some type_is_generic
      then Option.get type_is_generic
      else
        raise
          (Failure
            ("Invalid assignment from " ^ typ_to_string rvaluet ^ " to "
            ^ typ_to_string lvaluet))
  in
  let variable_table = { identifiers = StringMap.empty; parent = None } in
  let global_scope = ref variable_table in
  let rec find_identifier name (scope : symbol_table ref) =
    try StringMap.find name !scope.identifiers
    with Not_found -> (
      match !scope.parent with
      | Some parent -> find_identifier name (ref parent)
      | None ->
          raise
            (Failure ("The identifier " ^ name ^ " is not defined")))
  in
  let add_identifier name typ (scope : symbol_table ref) =
    try
      let _ = StringMap.find name !scope.identifiers in
      raise (Failure (" The identifier " ^ name ^ " has been already defined"))
    with Not_found ->
      scope :=
        {
          identifiers = StringMap.add name typ !scope.identifiers;
          parent = !scope.parent;
        }
  in
  let function_decls, overload_decls = List.fold_left add_func (built_in_decls, StringMap.empty) functions in
  (* find_func takes in a function's name and parameter types
     parameter types are only used if function is overloaded *)
  let find_func s arg_types = 
    let fun_decl_opt = StringMap.find_opt s function_decls in
    let overld_decls = StringMap.find_opt s overload_decls in
    (* let check_ts_equal is_eq t1 t2 = is_eq && (t1 = t2) in
    let match_args decl = List.fold_left2 check_ts_equal true arg_types (List.map (fun (t, _) -> t) decl.formals) in  *)
    let match_args decl = arg_types = (List.map (fun (t, _) -> t) decl.formals) in
    if Option.is_some fun_decl_opt then fun_decl_opt
    else if Option.is_some overld_decls
    then Some (try (List.find match_args (Option.get overld_decls)) with Not_found -> raise (Failure ("Calling of overloaded function " ^ s ^ " doesn't match existing signatures.")))
    else None 
  in
  let lambda_name =
    let counter = ref 0 in
    let next_id () =
      counter := !counter + 1;
      !counter
    in
    fun () -> "__lambda_" ^ string_of_int (next_id ()) ^ "__"
  in
  let rec check_expr expression scope =
    (* Returns true if the expression is a constant expression (value known at compile time) *)
    let rec check_const exp = 
      (match exp with 
      | Binop (e1, _, e2) -> (check_const e1) && (check_const e2)
      | Unop (_, e) -> check_const e
      | PrimLit _ -> true
      | ListLit _ -> false
      | TupleLit _ -> false
      | IfExpr (e1, e2, e3) -> (check_const e1) && (check_const e2) && (check_const e3)
      | Lambda _ -> true
      | Var _ -> false
      | Apply _ -> false
      | Call _ -> false
      | Noexpr -> false)
    in
    match expression with
    | PrimLit l -> (
        match l with
        | Int i -> (INT, SIntLit i)
        | Float f -> (FLOAT, SFloatLit f)
        | String s -> (STRING, SStringLit s)
        | Boolean b -> (BOOLEAN, SBoolLit b))
    | Noexpr -> (NONE, SNoExp)
    | Var s -> (find_identifier s scope, SVar s)
    | Unop (op, e) as ex ->
        let t, e' = check_expr e scope in
        let ty =
          match op with
          | NEG when t = INT || t = FLOAT -> t
          | NOT when t = BOOLEAN -> BOOLEAN
          | _ ->
              raise
                (Failure
                   ("illegal unary operator " ^ unop_to_string op
                  ^ typ_to_string t ^ " in " ^ expr_to_string ex))
        in
        (ty, SUnop (op, (t, e')))
    | Binop (e1, op, e2) as e ->
        let t1, e1' = check_expr e1 scope and t2, e2' = check_expr e2 scope in
        let same = t1 = t2 in
        let ty =
          match op with
          | (Add | Sub | Mul | Div) when same && t1 = INT -> INT
          | (Add | Sub | Mul | Div) when same && t1 = FLOAT -> FLOAT
          | (EQ | NEQ) when same -> BOOLEAN
          | (LTE | LT | GT | GTE) when same && (t1 = INT || t1 = FLOAT) ->
              BOOLEAN
          | (AND | OR) when same && t1 = BOOLEAN -> BOOLEAN
          | Mod when same && t1 = INT -> INT
          | _ ->
              raise
                (Failure
                   ("illegal binary operator " ^ typ_to_string t1 ^ " "
                  ^ biop_to_string op ^ " " ^ typ_to_string t2 ^ " in "
                  ^ expr_to_string e))
        in
        (ty, SBinop ((t1, e1'), op, (t2, e2')))
    | Lambda (binds, body) ->
        (* Code adapted from check_function down below *)
        let formals' = check_binds binds in
        let formals'' =
          List.fold_left
            (fun scope (typ, name) -> StringMap.add name typ scope)
            StringMap.empty formals'
        in
        let arg_types = List.map (fun (t, _) -> t) binds in
        let variable_table =
          { identifiers = formals''; parent = Some !global_scope }
        in
        let lambda_scope = ref variable_table in
        let t1, e1 = check_expr body lambda_scope
        in (LAMBDA (arg_types, t1), SLambda (lambda_name (), binds, (t1, e1)))
    | ListLit elements -> 
        (match elements with
        | [] -> (LIST None, SListLit (NONE, []))
        | (e :: es) -> 
            let first_t, first_sx = check_expr e scope in
            let sexp_rem = List.map (fun x -> check_expr x scope) es in
            let _ = if (List.for_all check_const elements) then () else raise (Failure "Non-constant expression or nested complex type literal found in a list literal") in
            if (List.for_all (fun (t, _) -> t = first_t ) sexp_rem) then (LIST (Some first_t), SListLit (first_t, ((first_t, first_sx) :: sexp_rem)))
            else
              raise
                (Failure
                   ("Illegal literal list with non-uniform types. First element has type " ^ typ_to_string first_t)))
    | TupleLit elements ->
        let sexp_list = List.map (fun x -> check_expr x scope) elements in
        let _ = if (List.for_all check_const elements) then () else raise (Failure "Non-constant expression or nested complex type literal found in a tuple literal") in
        let elem_types, _ = List.split sexp_list in
        (TUPLE (Some elem_types), STupleLit (elem_types, sexp_list))
    (* | TableLit _ -> (TUPLE, STupleLit ([], []))  *)
    (* | TableLit _ -> raise (Failure "table literals not currently implemented") TODO: This seems unfinished *)
    | Apply (obj, fname, args) -> 
        (* Resolves a rttype to ast typ in the context of the current calling argument types *)
        (* fallback_opt is needed esp when the ListElem'd list is an empty list (none type) *)
        let resolve_rttype rttyp argtypes fallback_opt = 
          let use_fallback fb = (match fb with Some t -> t | None -> raise (Failure "Call to a builtin function failed typechecking")) 
          in
          (match rttyp with
            P.Static t -> t
          | P.Relative i -> 
              let t = List.nth argtypes i
              in t
          | P.ListElem i ->
              let t = List.nth argtypes i
              in (match t with LIST (Some et) -> et | LIST None -> use_fallback fallback_opt  | _ -> raise (Failure ("ListElem relative type on something not a List " ^ typ_to_string t)))
          | P.ListWithElem i ->
              let t = List.nth argtypes i in
              let fail () = raise (Failure ("ListWithElem rttype cannot be used on complex types or lambda"))
              in (match t with LIST _ -> fail () | TABLE _ -> fail () | TUPLE _ -> fail () | LAMBDA _ -> fail () | t -> LIST (Some t))
          | P.TupleElem (i, j) -> 
              let t = List.nth argtypes i
              in (match t with TUPLE (Some ets) -> List.nth ets j | _ -> raise (Failure "TupleElem relative type on something not a Tuple"))
          | P.TableElem (i, j) ->
              let t = List.nth argtypes i
              in (match t with TABLE (Some ets) -> List.nth ets j | _ -> raise (Failure "TableElem relative type on something not a Table")))
        in
        if List.mem fname P.builtin_names then
          (* Evaluating the parameters first to get their type, then use that to check if it matches with declared rttypes *)
          let args_styp, args_sx = List.split (List.map (fun x -> check_expr x scope) (obj :: args)) in
          let (_, return_rttype, param_rttypes) = List.find (fun (n,_,_) -> fname = n) P.builtins in
          let decl_types = List.map (fun (rt, at) -> resolve_rttype rt args_styp (Some at)) (List.combine param_rttypes args_styp) in
          let check_asgn = List.map (fun (ft, et) -> check_assign ft et) (List.combine decl_types args_styp) in
          let args' = List.combine check_asgn args_sx in 
          (resolve_rttype return_rttype args_styp None, SCall (fname, args'))
        else
          check_expr (Call (fname, obj :: args)) scope
    | Call (fname, args) ->
        (* TODO: arg_types is definitely performing a redundant operation (evaling param expressions)  *)
        let arg_types = List.map (fun a -> let t, _ = check_expr a scope in t) args in
        let fdecl_opt = find_func fname arg_types
        in (match fdecl_opt with 
          | None ->
              let ltype = find_identifier fname scope in
              let args' = List.map (fun a -> check_expr a scope) args
              in (match ltype with LAMBDA (_, rt) -> (rt, SLamCall (fname, args')) | _ -> raise (Failure (fname ^ " is not a function or a variable lambda")))
          | Some fdecl ->
              let param_length = List.length fdecl.formals in
              (* TODO: Param types for var args are not checked *)
              if fdecl.is_vararg then
                if List.length args < param_length then
                  raise
                    (Failure
                      ("Function " ^ fdecl.fname ^ " requires at least "
                      ^ string_of_int param_length ^ " arguments"))
                else
                  let rec first_n ls n =
                    if n == 0 then []
                    else
                      match ls with
                      | [] ->
                          raise (Failure "first_n list smaller than specified length")
                      | v :: vs -> v :: first_n vs (n - 1)
                  in
                  let check_call (ft, _) e =
                    let et, e' = check_expr e scope in
                    (check_assign ft et, e')
                  in
                  (* TODO: messy code for checking and evaluating varargs, but this will do for now *)
                  (* leaving this here just for type checking *)
                  let _ =
                    List.map2 check_call fdecl.formals (first_n args param_length)
                  in
                  let eval_params e =
                    let et, e' = check_expr e scope in
                    (et, e')
                  in
                  let vargs = List.map eval_params args in
                  (fdecl.typ, SCall (fname, vargs))
              else if List.length args != param_length then
                raise (Failure "Arguments-Parameters MisMatch")
              else
                let check_call (ft, _) e =
                  let et, e' = check_expr e scope in
                  (check_assign ft et, e')
                in
                let args' = List.map2 check_call fdecl.formals args in
                (fdecl.typ, SCall (fname, args')))
    | IfExpr (e1, e2, e3) as e -> (
        let t1, e1' = check_expr e1 scope
        and t2, e2' = check_expr e2 scope
        and t3, e3' = check_expr e3 scope in
        match (t1, t2 = t3) with
        | BOOLEAN, true -> (t2, SIfExpr ((t1, e1'), (t2, e2'), (t3, e3')))
        | _ ->
            raise
              (Failure
                 ("illegal if expression " ^ typ_to_string t1 ^ " "
                ^ typ_to_string t2 ^ " " ^ " " ^ typ_to_string t3 ^ " in "
                ^ expr_to_string e)))
  in
  let check_bool_expr e scope =
    let t', e' = check_expr e scope
    and err = "expected Boolean expression in " ^ expr_to_string e in
    if t' != BOOLEAN then raise (Failure err) else (t', e')
  in
  let rec check_stmt statement function_decl scope =
    match statement with
    | Block sl ->
        let block_variable_table =
          { identifiers = StringMap.empty; parent = Some !scope }
        in
        let block_scope = ref block_variable_table in
        let rec check_stmt_list statement_list =
          match statement_list with
          | [ Return _ as s ] -> [ check_stmt s function_decl block_scope ]
          | Return _ :: _ -> raise (Failure "Nothing may follow a return")
          | Block s :: ss -> check_stmt_list (s @ ss)
          | s :: ss -> 
            let st = check_stmt s function_decl block_scope in 
            let st_list = check_stmt_list ss in
            st::st_list 
          | [] -> []
        in
        SBlock (check_stmt_list sl)
    | Expr e -> SExpr (check_expr e scope)
    | If (p, b1, b2) ->
        let p' = check_bool_expr p scope
        and b1' = check_stmt b1 function_decl scope
        and b2' = check_stmt b2 function_decl scope in
        SIf (p', b1', b2')
    | While (p, s) ->
        let p' = check_bool_expr p scope
        and s' = check_stmt s function_decl scope in
        SWhile (p', s')
    | Declare (t, id, e) ->
        let et', e' = check_expr e scope in
        let same_type = t = et' in
        let flex_typing =
        (match et' with 
          LIST None -> (match t with LIST _ -> true | _ -> false) 
          | TUPLE None -> (match t with TUPLE _ -> true | _ -> false)
          | TABLE None -> (match t with TABLE _ -> true | _ -> false)
          | _ -> false) in
        let empty_declaration = et' = NONE in
        if (same_type || flex_typing) then
          let _ = add_identifier id t scope in
          SDeclare (t, id, (et', e'))
        else if empty_declaration then
          let (et', e') =
            match t with
            | INT -> (INT, SIntLit 0)
            | FLOAT -> (FLOAT, SFloatLit 0.0)
            | STRING -> (STRING, SStringLit "")
            | BOOLEAN -> (BOOLEAN, SBoolLit false)
            | _ -> raise (Failure ("The " ^ typ_to_string t ^ " doesn't support empty variable declaration." ))
          in
          let _ = add_identifier id t scope in
          SDeclare (t, id, (et', e'))
        else
          raise
            (Failure
               ("Invalid Declaration of identifier. Expected: "
              ^ typ_to_string t ^ " Got: " ^ typ_to_string et' ^ " in "
              ^ expr_to_string e))
    | Return e ->
        let isfunction = function_decl.fname = "_" in
        if isfunction then
          raise (Failure "Can not use return outside of function.")
        else
          let t, e' = check_expr e scope in
          let same_type = t = function_decl.typ in
          if same_type then SReturn (t, e')
          else raise (Failure "The function return type mismatch.")
    | Assign (s, e) ->
        let lt = find_identifier s scope and rt, e' = check_expr e scope in
        let flex_typing =
        (match rt with 
          LIST None -> (match lt with LIST _ -> true | _ -> false) 
          | TUPLE None -> (match lt with TUPLE _ -> true | _ -> false)
          | TABLE None -> (match lt with TABLE _ -> true | _ -> false)
          | _ -> false) in
        if (rt = lt) || flex_typing then SAssign (s, (rt, e'))
        else
          raise
            (Failure
               ("Illegal assignment of " ^ typ_to_string lt ^ " and "
              ^ typ_to_string rt))
    | Break -> SBreak
  in
  let check_function func =
    let decl = Option.get (find_func func.fname (List.map (fun (t, _) -> t) func.formals)) in
    let formals' = check_binds decl.formals in
    let formals'' =
      List.fold_left
        (fun scope (typ, name) -> StringMap.add name typ scope)
        StringMap.empty formals'
    in
    let function_variable_table =
      { identifiers = formals''; parent = Some !global_scope }
    in
    let function_scope = ref function_variable_table
    in
    {
      styp = decl.typ;
      sfname = decl.fname;
      ov_orig_name = if decl.is_overload then Some func.fname else None;
      sformals = formals';
      sbody = 
      (match check_stmt (Block decl.body) func function_scope with
        | SBlock sl -> sl
        | _ -> raise (Failure "Internal Error: Block did not become block"));
    }
  in
  ( List.map check_function functions,
    List.map
      (fun st ->
        check_stmt st
          {
            typ = NONE;
            fname = "_";
            formals = [];
            body = [];
            is_vararg = false;
            is_overload = false;
          }
          global_scope)
      statements )
