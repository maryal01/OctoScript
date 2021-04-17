open Ast
open Sast
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)


type symbol_table = {
  identifiers : typ StringMap.t;
  parent : symbol_table option;
}

let check (functions, statements) =
  let check_binds (to_check : bind list) =
    let check_it checked binding =
      let void_err = "illegal void "  ^ snd binding
      and dup_err = "duplicate "   ^ snd binding in
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
    let add_bind map (name, ty) =
      StringMap.add name
        { typ = NONE; fname = name; formals = [ (ty, "x") ]; body = [] }
        map
    in
    List.fold_left add_bind StringMap.empty
      [
        ("print", STRING); ("printb", BOOLEAN); ("printf", FLOAT); ("printbig", INT);
      ]
  in
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname in
    match fd with
    | _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ -> StringMap.add n fd map
  in
  let check_assign lvaluet rvaluet =
    if lvaluet = rvaluet then lvaluet else raise (Failure "Invalid assignment")
  in
  let id_table = { identifiers = StringMap.empty; parent = None } in
  let rec find_identifier name scope =
    try StringMap.find name scope.identifiers
    with Not_found -> (
      match scope.parent with
      | Some parent -> find_identifier name parent
      | None -> raise (Failure "The identifier is not already defined. "))
  in
  let add_identifier name typ scope =
    try
      let _ = StringMap.find name scope.identifiers in
      raise (Failure " The identifier has been already defined")
    with Not_found ->
      scope
      = {
          identifiers = StringMap.add name typ scope.identifiers;
          parent = scope.parent;
        }
  in
  let function_decls = List.fold_left add_func built_in_decls functions in
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in
  (* let rec extract_unbound formal_names expression unbound =
    let ext = extract_unbound formal_names in
    (match expression with 
        Binop (e1, _, e2) -> 
          let ub1 = ext e1 unbound
          in ext e2 ub1
      | Unop (_, e) -> ext e unbound 
      | PrimLit _ -> unbound
      | ListLit _ -> unbound
      | TupleLit _ -> unbound
      | TableLit _ -> unbound
      | IfExpr (e1, e2, e3) ->
          let ub1 = ext e1 unbound in
          let ub2 = ext e2 ub1 
          in ext e3 ub2
      | Lambda (bs, e) -> 
          let fml = List.map (fun (_, n) -> n) bs
          in extract_unbound fml e unbound
      | Var n -> 
          (if (List.mem n formal_names) 
          then StringSet.add n unbound
          else unbound)
      | Apply (e, _, ps) -> 
          List.fold_left (fun s e -> ext e s) unbound (e :: ps)
      | Call (_, ps) ->
          (* TODO: not sure how calls to lambdas stored in vars 
             are handled up to this point *)
          (* TODO: I'm just not going to worry about nested lambda calls at this point
             but this could very well be a problem here *)
          List.fold_left (fun s e -> ext e s) unbound ps
      | Noexpr -> unbound)
  in *)
  let lambda_name = 
    let counter = ref 0 in
    let next_id = fun () -> counter := (!counter) + 1; !counter in
    fun () -> "__lambda_" ^ (string_of_int (next_id ())) ^ "__"
  in
  let rec check_expr expression scope =
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
          | Pow when same && t1 = INT -> INT
          | Log when same && t1 = INT -> FLOAT
          | Mod when same && t1 = INT -> INT
          | _ ->
              raise
                (Failure
                   ("illegal binary operator " ^ typ_to_string t1 ^ " "
                  ^ biop_to_string op ^ " " ^ typ_to_string t2 ^ " in "
                  ^ expr_to_string e))
        in
        (ty, SBinop ((t1, e1'), op, (t2, e2')))
    | Lambda (args, e) ->
        let t1, e1 = check_expr e scope in
        (* let _, formal_names = List.split args in *)
        (* let unbound = extract_unbound formal_names e StringSet.empty in  *)
        (t1, SLambda (lambda_name (), args, (t1, e1)))
    | ListLit elements as list -> (
        match elements with
        | [] -> (NONE, SListLit (NONE, elements))
        | elem :: elems -> (
            let ex = PrimLit elem in
            let t1, _ = check_expr ex scope in (* check why e' not needed?*)
            let all_func elem' =
              let t', _ = check_expr (PrimLit elem') scope in (* check why e' not needed?*)
              t1 = t'
            in
            match List.for_all all_func elems with
            | true -> (t1, SListLit (t1, elements))
            | false ->
                raise
                  (Failure
                     ("illegal List literal " ^ typ_to_string t1 ^ " expected "
                    ^ " in " ^ expr_to_string list))))
    | TupleLit elements ->
        let fold_func elem =
          let t1, _ = check_expr (PrimLit elem) scope in (* check why e' not needed?*)
          t1
        in
        let typ_list = List.map fold_func elements in
        (TUPLE, STupleLit (typ_list, elements))
    | TableLit _ -> (TUPLE, STupleLit ([], []))
    | Apply (obj, fname, args) -> check_expr (Call (fname, obj :: args)) scope
    | Call (fname, args) ->
      let fdecl = find_func fname in
      let param_length = List.length fdecl.formals in
          if List.length args != param_length then
            raise (Failure ("Arguments-Parameters MisMatch"))
          else let check_call (ft, _) e = 
            let (et, e') = check_expr e scope
            in (check_assign ft et, e')
          in 
          let args' = List.map2 check_call fdecl.formals args
          in (fdecl.typ, SCall(fname, args'))
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
  let rec check_stmt statement scope =
    match statement with
    | Block [] -> SBlock []
    | Block sl ->
        let rec check_stmt_list statement_list block_scope =
          match statement_list with
          | [ (Return _ as s) ] -> [ check_stmt s block_scope ]
          | Return _ :: _ -> raise (Failure "Nothing may follow a return")
          | Block s :: ss -> check_stmt_list (s @ ss) block_scope
          | s :: ss ->
              check_stmt s block_scope :: check_stmt_list ss block_scope
          | [] -> []
        in
        SBlock
          (check_stmt_list sl
             { identifiers = StringMap.empty; parent = Some scope })
    | Expr e -> SExpr (check_expr e scope)
    | If (p, b1, b2) ->
        let p' = check_bool_expr p scope
        and b1' = check_stmt b1 scope
        and b2' = check_stmt b2 scope in
        SIf (p', b1', b2')
    | While (p, s) ->
        let p' = check_bool_expr p scope and s' = check_stmt s scope in
        SWhile (p', s')
    | Declare (t, id, e) ->
        let et', e' = check_expr e scope in
        let same_type = t = et' in
        if same_type then
          let _ = add_identifier id t scope in
          SDeclare (t, id, (et', e'))
        else raise (Failure "Invalid Declaration of identifier")
        (* TODO: think about declaring lambda, table for READ, none *)
    | Return e ->
        let t, e' = check_expr e scope in
        (*TODO: if t = func.typ then *) SReturn (t, e')
    | Assign (s, e) ->
        let lt = find_identifier s scope and rt, e' = check_expr e scope in
        if rt = lt then SAssign (s, (rt, e'))
        else
          raise
            (Failure
               ("Illegal assignment of " ^ typ_to_string lt ^ " and "
              ^ typ_to_string rt))
    | Break -> SBreak
  in
  let check_function func =
    let formals' = check_binds func.formals in
    let formals'' =
      List.fold_left
        (fun scope (typ, name) -> StringMap.add name typ scope)
        StringMap.empty formals'
    in
    let func_scope = { identifiers = formals''; parent = Some id_table } in
    {
      styp = func.typ;
      sfname = func.fname;
      sformals = formals';
      sbody =
        (match check_stmt (Block func.body) func_scope with
        | SBlock sl -> sl
        | _ -> raise (Failure "Internal Error: Block did not become block"));
    }
  in
  ((List.map check_function functions), (List.map (fun st -> check_stmt st id_table) statements))