module L = Llvm
module A = Ast
open Sast
module P = Predef
module StringMap = Map.Make (String)

(* TODO: Test creating and evocation of lambdas, especially passed to a C predef function *)
(* TODO: Implement built in functions within ocaml; get, len, type conversions? *)

let translate (functions, statements) =
  let context = L.global_context () in
  let i32_t = L.i32_type context
  and i8_t = L.i8_type context (* char *)
  and i1_t = L.i1_type context (* bool *)
  and float_t = L.double_type context
  and void_t = L.void_type context
  and the_module = L.create_module context "OctoScript" in

  let ltype_of_typ = function
    | A.INT -> i32_t
    | A.BOOLEAN -> i1_t
    | A.FLOAT -> float_t
    | A.NONE -> void_t
    | A.STRING -> L.pointer_type i8_t
    | A.LAMBDA -> L.pointer_type i8_t
    | A.TABLE -> L.pointer_type i8_t
    | A.TUPLE -> L.pointer_type i8_t
    | A.LIST -> L.pointer_type i8_t
  in

  let program =
    { styp = A.NONE; sfname = "main"; sformals = []; sbody = statements }
    :: functions
  in
  let predef_decls : (L.llvalue * A.typ) StringMap.t =
    let predef_type rt ps =
      match ps with
      | P.Fixed ps ->
          L.function_type (ltype_of_typ rt)
            (Array.of_list (List.map (fun p -> ltype_of_typ p) ps))
      | P.Var ps ->
          L.var_arg_function_type (ltype_of_typ rt)
            (Array.of_list (List.map (fun p -> ltype_of_typ p) ps))
    in
    let predef_decl m (on, cn, rt, ps) =
      if on = "length" then m
      else
        StringMap.add on
          (L.declare_function cn (predef_type rt ps) the_module, rt)
          m
    in
    List.fold_left predef_decl StringMap.empty P.predefs
  in

  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) fdecl.sformals)
      in
      let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m
    in
    List.fold_left function_decl StringMap.empty program
  in

  let extract_lambda ls fdecl =
    let rec ext_sx (_, e) ls =
      match e with
      | SIntLit _ -> ls
      | SFloatLit _ -> ls
      | SStringLit _ -> ls
      | SBoolLit _ -> ls
      | SListLit _ -> ls
      | STupleLit _ -> ls
      | STableLit _ -> ls
      | SBinop (e1, _, e2) ->
          let ls' = ext_sx e1 ls in
          ext_sx e2 ls'
      | SUnop (_, e) -> ext_sx e ls
      | SIfExpr (cond, e1, e2) ->
          let ls' = ext_sx cond ls in
          let ls'' = ext_sx e1 ls' in
          ext_sx e2 ls''
      | SLambda (s, bs, e) -> (s, bs, e) :: ls
      | SVar _ -> ls
      | SCall (_, ps) -> List.fold_right ext_sx ps ls
      | SNoExp -> ls
    in
    let rec ext_stmt s ls =
      match s with
      | SBlock sl -> List.fold_right ext_stmt sl ls
      | SWhile (cond, s) ->
          let ls' = ext_sx cond ls in
          ext_stmt s ls'
      | SIf (cond, s1, s2) ->
          let ls' = ext_sx cond ls in
          let ls'' = ext_stmt s1 ls' in
          ext_stmt s2 ls''
      | SReturn e -> ext_sx e ls
      | SBreak -> ls
      | SDeclare (_, _, e) -> ext_sx e ls
      | SAssign (_, e) -> ext_sx e ls
      | SExpr e -> ext_sx e ls
    in
    ext_stmt (SBlock fdecl.sbody) [] @ ls
  in
  let extracted_lambdas = List.fold_left extract_lambda [] program in
  let lambda_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let lam_func =
      List.map
        (fun (n, bs, (t, e)) ->
          { styp = t; sfname = n; sformals = bs; sbody = [ SExpr (t, e) ] })
        extracted_lambdas
    in
    let lambda_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) fdecl.sformals)
      in
      let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m
    in
    List.fold_left lambda_decl StringMap.empty lam_func
  in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let the_function, _ =
      try StringMap.find fdecl.sfname function_decls
      with Not_found -> raise (Failure "build body func")
    in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let formal_env =
      let add_formal m (t, n) p =
        let () = L.set_value_name n p in
        let local = L.build_alloca (ltype_of_typ t) n builder in
        let _ = L.build_store p local builder in
        StringMap.add n (t, local) m
      in
      List.fold_left2 add_formal StringMap.empty fdecl.sformals
        (Array.to_list (L.params the_function))
    in

    (* Return the value for a variable or formal argument. First check
       * locals, then globals *)
    let lookup n env =
      let _, v =
        try StringMap.find n env
        with Not_found -> raise (Failure ("lookup for " ^ n ^ " failed"))
      in
      v
    in

    (* Complex type memeory layout:
        List  : int length, char type, data...
        Tuple : int length, char type[lenght], data...
        Table : Same as a list containing tuples
    *)

    (* Construct code for an expression; return its value *)
    (* NOTE: expr is guaranteed to not modify the env *)
    let rec expr builder env ((_, e) : sexpr) = 
      let rexpr = expr builder env in
      let global_str s n = L.build_global_stringptr s n builder in
      let mk_int i = L.const_int i32_t i in 
      (* let ltype_of_typs ts = Array.of_list (List.map ltype_of_typ ts) in *)
      let lval_of_prim p = 
        (match p with 
            A.Int     i -> mk_int i
          | A.Float   f -> L.const_float float_t f
          | A.String  s -> global_str s "string"
          | A.Boolean b -> L.const_int i1_t (if b then 1 else 0))
      and type_sym t = 
        (match t with 
            A.INT     -> mk_int 0
          | A.BOOLEAN -> mk_int 1
          | A.FLOAT   -> mk_int 2
          | A.STRING  -> mk_int 3
          | A.LAMBDA  -> mk_int 4
          | A.LIST    -> mk_int 10
          | A.TUPLE   -> mk_int 11
          | A.TABLE   -> raise (Failure "TABLE should be represented as a LIST of TUPLES")
          | A.NONE    -> raise (Failure "NONE type cannot be an element of a complex type"))
      and mallocate llval = 
          let v = L.build_malloc (L.type_of llval) "alc_tmp" builder in
          let _ = L.build_store llval v builder
          in v
      in (match e with 
        SIntLit i     -> lval_of_prim (A.Int i)
      | SFloatLit f   -> lval_of_prim (A.Float f)
      | SStringLit s  -> lval_of_prim (A.String s)
      | SBoolLit b    -> lval_of_prim (A.Boolean b)
      | SListLit (t, ps) -> 
          let len = L.const_int i32_t (List.length ps) in
          let content =  (type_sym A.LIST) :: (len :: ((type_sym t) :: List.map lval_of_prim ps)) in
          let value = L.const_struct context (Array.of_list content)  
          in mallocate value
      | STupleLit (ts, ps) -> 
          let len = L.const_int i32_t (List.length ps) in
          let types = List.map type_sym ts in
          let content = (type_sym A.TUPLE) :: (len :: (types @ (List.map lval_of_prim ps))) in
          let value = L.const_struct context (Array.of_list content) 
          in mallocate value
      | STableLit (ts, pss) -> 
          let num_rows = L.const_int i32_t (List.length pss) in
          let row_data = List.map (fun row -> rexpr (A.TUPLE, (STupleLit (ts, row)))) pss in
          let content = [(type_sym A.LIST); num_rows; type_sym A.TUPLE] @ (row_data) in
          let value = L.const_struct context (Array.of_list content)
          in mallocate value
      | SBinop (e1, op, e2) -> 
          let (t, _) = e1
            and e1' = rexpr e1
            and e2' = rexpr e2
            and raise_typerr op t = raise (Failure ("Internal error: " ^ op ^ " with " ^ t ^ " operands not allowed"))
          in
          (match t with 
              A.INT -> 
              (match op with
                  A.AND -> raise_typerr "AND" "int"
                | A.OR ->  raise_typerr "OR" "int"
                | A.Add -> L.build_add
                | A.Sub -> L.build_sub
                | A.Mul -> L.build_mul
                | A.Div -> L.build_sdiv
                | A.Pow -> raise(Failure("pow for int is not impleemented"))
                | A.Log -> raise(Failure("log for int is not impleemented")) (* TODO: no operator in LLVM so maybe make this an actual function call? *)
                | A.GT -> L.build_icmp L.Icmp.Sgt
                | A.GTE -> L.build_icmp L.Icmp.Sge
                | A.LT -> L.build_icmp L.Icmp.Slt
                | A.LTE -> L.build_icmp L.Icmp.Sle
                | A.EQ -> L.build_icmp L.Icmp.Eq
                | A.NEQ -> L.build_icmp L.Icmp.Ne
                | A.Mod -> L.build_srem)
            | A.FLOAT ->                  
                (match op with
                  A.AND -> raise_typerr "AND" "float"
                | A.OR ->  raise_typerr "OR" "float"
                | A.Add -> L.build_fadd
                | A.Sub -> L.build_fsub
                | A.Mul -> L.build_fmul
                | A.Div -> L.build_fdiv
                | A.Pow -> raise(Failure("pow for float is not impleemented"))
                | A.Log -> raise(Failure("log for float is not impleemented")) (* TODO: no operator in LLVM so maybe make this an actual function call? *)
                | A.GT -> L.build_fcmp L.Fcmp.Ogt
                | A.GTE -> L.build_fcmp L.Fcmp.Oge
                | A.LT -> L.build_fcmp L.Fcmp.Olt
                | A.LTE -> L.build_fcmp L.Fcmp.Ole
                | A.EQ -> L.build_fcmp L.Fcmp.Oeq
                | A.NEQ -> L.build_fcmp L.Fcmp.One
                | A.Mod -> L.build_frem)
            | A.BOOLEAN -> 
              (match op with
                  A.AND -> L.build_and
                | A.OR ->  L.build_or
                | A.EQ -> L.build_icmp L.Icmp.Eq
                | A.NEQ -> L.build_icmp L.Icmp.Ne
                | _ -> raise_typerr "ARITHMETIC OP" "bool")
            | _ -> raise_typerr "BINOP" "non-int/float/bool"
        ) e1' e2' "tmp" builder
      | SUnop(op, e) -> 
          let (t, _) = e in
          let e' = rexpr e in
          (match op with 
              A.NOT when t = A.BOOLEAN -> L.build_not
            | A.NEG when t = A.FLOAT -> L.build_fneg
            | A.NEG when t = A.INT -> L.build_neg
            | _ -> raise (Failure "Internal Error: Not a unary operator")
          ) e' "tmp" builder
      (* additional logic required here to cast complex types into pointers *)
      | SVar s -> L.build_load (lookup s env) s builder
      | SIfExpr (cond, e1, e2) -> 
          let cond' = rexpr cond in
          let e1' = rexpr e1 in
          let e2' = rexpr e2 in
          L.build_select cond' e1' e2' "tmp" builder 
      | SLambda (n, _, _) -> expr builder env (A.LAMBDA, SStringLit n)
      | SCall (f, args) -> 
          let cast_complex (t, sx) = 
            let v = rexpr (t, sx) in
              (match t with 
                  A.LIST -> L.build_bitcast v (L.pointer_type i8_t) "var_list_tmp" builder
                | A.TUPLE -> L.build_bitcast v (L.pointer_type i8_t) "var_tuple_tmp" builder
                | A.TABLE -> L.build_bitcast v (L.pointer_type i8_t) "var_table_tmp" builder
                | _ -> v) 
          in 
          if f = "length" then
            let listt = rexpr (List.hd args) 
            in L.build_load (L.build_struct_gep listt 1 "tmp2" builder) "tmp3" builder
          else if f = "get" then
            let value = List.hd (List.tl args) in
            let idx = (expr builder env value) in 
            let listt = rexpr (List.hd args)
            in
            let inner_list = L.build_struct_gep listt 3 "temp" builder in
            L.build_load
              (L.build_gep inner_list [| idx |] "tmp2" builder)
              "tmp3" builder
          else
          let llargs = List.map cast_complex args in           
          let userdef dom =
            let (fdef, fdecl) = (try StringMap.find f dom with Not_found -> raise (Failure (f ^ " is not a declared function"))) in
            let result = (match fdecl.styp with
                            A.NONE -> ""
                          | _ -> f ^ "_result") in
              L.build_call fdef (Array.of_list llargs) result builder
          and predef f =
            let (pdecl, rt) = (try StringMap.find f predef_decls with Not_found -> raise (Failure (f ^ " is not a recognized built-in function"))) in
            let result = (match rt  with
                            A.NONE -> ""
                          | _ -> f ^ "_result") in
            L.build_call pdecl (Array.of_list llargs) result builder
          and is_lambda = StringMap.mem f env
          and is_predef = List.mem f P.predef_names in
          if is_lambda then userdef lambda_decls
          else if is_predef then predef f
          else userdef function_decls
      | SNoExp -> L.const_null void_t)
      (* Actually not quite sure? *)
    in

    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
      | Some _ -> ()
      | None -> ignore (instr builder)
    in
    (* return a builder env tuple *)
    let rec stmt builder env = function
        SBlock sl -> 
          let (_, b) = List.fold_left (fun (e, b) s -> stmt b e s) (env, builder) sl
          in (env, b)
      | SWhile (cond, s) -> 
          (* some work for scoping *)
          let pred_bb = L.append_block context "while_cond" the_function in
            let _ = L.build_br pred_bb builder in
          let body_bb = L.append_block context "while_body" the_function in
            let (_, while_builder) = stmt (L.builder_at_end context body_bb) env s in
            let () = add_terminal while_builder (L.build_br pred_bb) in
          let pred_builder = L.builder_at_end context pred_bb in
          let bool_val = expr pred_builder env cond in
          let merge_bb = L.append_block context "merge" the_function in
          let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
          (env, L.builder_at_end context merge_bb)
      | SIf (cond, s1, s2) ->
          let bool_val = expr builder env cond in
          let merge_bb = L.append_block context "merge" the_function in
            let branch_instr = L.build_br merge_bb in
          let then_bb = L.append_block context "then" the_function in
            let (_, then_builder) = stmt (L.builder_at_end context then_bb) env s1 in
            let () = add_terminal then_builder branch_instr in
          let else_bb = L.append_block context "else" the_function in
            let (_, else_builder) = stmt (L.builder_at_end context else_bb) env s2 in
            let () = add_terminal else_builder branch_instr in
          let _ = L.build_cond_br bool_val then_bb else_bb builder in
          (env, L.builder_at_end context merge_bb)
      | SReturn e -> 
          let cast_complex (t, sx) = 
            let v = expr builder env (t, sx) in
              (match t with 
                  A.LIST -> L.build_bitcast v (L.pointer_type i8_t) "var_list_tmp" builder
                | A.TUPLE -> L.build_bitcast v (L.pointer_type i8_t) "var_tuple_tmp" builder
                | A.TABLE -> L.build_bitcast v (L.pointer_type i8_t) "var_table_tmp" builder
                | _ -> v) in 
          let _ = 
            (match fdecl.styp with
                A.NONE -> L.build_ret_void builder 
              | _ -> L.build_ret (cast_complex e) builder)
          in (env, builder)
      | SBreak -> raise(Failure("break is not impleemented"))
      | SDeclare (t, n, e) -> 
          let e' = expr builder env e in
          let v = L.build_alloca (L.type_of e') n builder in
          let env' = StringMap.add n (t, v) env in
          let _ = L.build_store e' v builder
          in (env', builder)
      | SAssign (n, e) ->
          let e' = expr builder env e in
          let _ = L.build_store e' (lookup n env) builder 
          in (env, builder) 
      | SExpr e -> let _ = expr builder env e in (env, builder) 
    in

    let _, builder = stmt builder formal_env (SBlock fdecl.sbody) in
    add_terminal builder
      (match fdecl.styp with
      | A.NONE -> L.build_ret_void
      | A.FLOAT -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in
  List.iter build_function_body program;
  the_module
