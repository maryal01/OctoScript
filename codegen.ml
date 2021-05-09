module L = Llvm
module A = Ast
open Sast
module P = Predef
module StringMap = Map.Make (String)

(* TODO: Test creating and evocation of lambdas, especially passed to a C predef function *)
(* TODO: Implement built in functions within ocaml; get, len, type conversions? *)

let translate (functions, statements) =
  (* let _ = L.enable_pretty_stacktrace in *)
  let context = L.global_context () in
  let i32_t = L.i32_type context
  and i8_t = L.i8_type context (* char *)
  and i1_t = L.i1_type context (* bool *)
  and float_t = L.double_type context
  and void_t = L.void_type context
  and the_module = L.create_module context "OctoScript" in

  let rec ltype_of_typ = function
    | A.INT -> i32_t
    | A.BOOLEAN -> i1_t
    | A.FLOAT -> float_t
    | A.NONE -> void_t
    | A.STRING -> L.pointer_type i8_t
    | A.LAMBDA (pts, rt) ->
        L.pointer_type
          (L.function_type (ltype_of_typ rt)
             (Array.of_list (List.map ltype_of_typ pts)))
    | A.TABLE _ -> L.pointer_type i8_t
    | A.TUPLE _ -> L.pointer_type i8_t
    | A.LIST _ -> L.pointer_type i8_t
  in
  let lval_of_type_size = function
    | A.INT -> L.const_int i32_t 4
    | A.BOOLEAN -> L.const_int i32_t 1
    | A.FLOAT -> L.const_int i32_t 8
    | A.NONE -> raise (Failure "None type shouldnt be called here")
    | A.STRING -> L.const_int i32_t 8
    | A.LAMBDA _ -> L.const_int i32_t 8
    | A.TABLE _ -> L.const_int i32_t 8
    | A.TUPLE _ -> L.const_int i32_t 8
    | A.LIST _ -> L.const_int i32_t 8
  in
  let program =
    {
      styp = A.NONE;
      sfname = "main";
      sformals = [];
      sbody = statements;
      ov_orig_name = None;
    }
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
      (* let _ = L.dump_type ftype in *)
      StringMap.add name (L.define_function name ftype the_module, fdecl) m
    in
    List.fold_left function_decl StringMap.empty program
  in

  (* String map mapping from written name -> decl list of overloaded function *)
  let overload_decls =
    let add_decl k v m =
      if StringMap.mem k m then StringMap.add k (v :: StringMap.find k m) m
      else StringMap.add k [ v ] m
    in
    let map_ov m decl =
      match decl.ov_orig_name with Some n -> add_decl n decl m | None -> m
    in
    List.fold_left map_ov StringMap.empty program
  in
  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
    | Some _ -> ()
    | None -> ignore (instr builder)
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
      | SLambda (s, bs, e) -> ext_sx e ((s, bs, e) :: ls)
      | SVar _ -> ls
      | SCall (_, ps) -> List.fold_right ext_sx ps ls
      | SLamCall _ -> ls
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
          {
            styp = t;
            sfname = n;
            sformals = bs;
            sbody = [ SReturn (t, e) ];
            ov_orig_name = None;
          })
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
      with Not_found -> (
        try StringMap.find fdecl.sfname lambda_decls
        with Not_found -> raise (Failure "build body func"))
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

    (* LIST CONCAT FUNCTION *)
    let build_list_concat_function ty =
      let mk_int i = L.const_int i32_t i in
      let list_struct_type =
        L.packed_struct_type context [| i32_t; i32_t; i32_t; ltype_of_typ ty |]
      in
      let list_struct_ptr = L.pointer_type list_struct_type in
      let list_concat_function =
        L.define_function
          ("_LIST_CONCAT_" ^ A.typ_to_string ty)
          (L.function_type list_struct_ptr
             (Array.of_list [ list_struct_ptr; list_struct_ptr ]))
          the_module
      in
      let list_concat_function_builder =
        L.builder_at_end context (L.entry_block list_concat_function)
      in
      let listt1 = L.param list_concat_function 0 in
      let listt2 = L.param list_concat_function 1 in
      let data1 =
        L.build_struct_gep listt1 3 "tmp_data" list_concat_function_builder
      in
      let casted_data1 =
        L.build_bitcast data1
          (L.pointer_type (ltype_of_typ ty))
          "tmp_l_source_data_cast1" list_concat_function_builder
      in
      let data2 =
        L.build_struct_gep listt2 3 "tmp_data" list_concat_function_builder
      in
      let casted_data2 =
        L.build_bitcast data2
          (L.pointer_type (ltype_of_typ ty))
          "tmp_l_source_data_cast2" list_concat_function_builder
      in
      let length1 =
        L.build_load
          (L.build_struct_gep listt1 1 "tmp" list_concat_function_builder)
          "tmp" list_concat_function_builder
      in
      let length2 =
        L.build_load
          (L.build_struct_gep listt2 1 "tmp" list_concat_function_builder)
          "tmp" list_concat_function_builder
      in
      let new_length =
        L.build_add length1 length2 "tmp_new_len" list_concat_function_builder
      in
      let struct_size =
        L.build_add (mk_int 12)
          (L.build_mul new_length (lval_of_type_size ty) "tmp_new_size"
             list_concat_function_builder)
          "tmp_struct_size" list_concat_function_builder
      in
      let struct_malloc =
        L.build_array_malloc i8_t struct_size "new_struct_malloc"
          list_concat_function_builder
      in
      let new_casted_struct =
        L.build_bitcast struct_malloc
          (L.pointer_type list_struct_type)
          "tmp_l_cast" list_concat_function_builder
      in
      let new_data =
        L.build_struct_gep new_casted_struct 3 "tmp_data"
          list_concat_function_builder
      in
      let new_casted_data =
        L.build_bitcast new_data
          (L.pointer_type (ltype_of_typ ty))
          "tmp_l_dest_data_cast" list_concat_function_builder
      in
      let _ =
        L.build_store
          (L.build_load
             (L.build_struct_gep listt1 0 "tmp" list_concat_function_builder)
             "tmp_0_load" list_concat_function_builder)
          (L.build_struct_gep new_casted_struct 0 "tmp"
             list_concat_function_builder)
          list_concat_function_builder
      in
      let _ =
        L.build_store new_length
          (L.build_struct_gep new_casted_struct 1 "tmp"
             list_concat_function_builder)
          list_concat_function_builder
      in
      let _ =
        L.build_store
          (L.build_load
             (L.build_struct_gep listt1 2 "tmp" list_concat_function_builder)
             "tmp_0_store" list_concat_function_builder)
          (L.build_struct_gep new_casted_struct 2 "tmp"
             list_concat_function_builder)
          list_concat_function_builder
      in
      (* building loop1 *)
      let iterator =
        L.build_alloca i32_t "alloc_iter" list_concat_function_builder
      in
      let _ = L.build_store (mk_int 0) iterator list_concat_function_builder in

      let pred_bb = L.append_block context "while_cond" list_concat_function in
      let body_bb = L.append_block context "while_body" list_concat_function in
      let merge_bb = L.append_block context "merge" list_concat_function in
      let while_builder = L.builder_at_end context body_bb in
      let merge_builder = L.builder_at_end context merge_bb in
      let pred_builder = L.builder_at_end context pred_bb in
      let _ = L.build_br pred_bb list_concat_function_builder in

      (* body basic block  1 *)
      let idx1 = L.build_load iterator "idx1" while_builder in
      let old_data1 =
        L.build_load
          (L.build_gep casted_data1 [| idx1 |] "data1" while_builder)
          "tmp_load" while_builder
      in
      let new_box1 =
        L.build_gep new_casted_data [| idx1 |] "tmp_set_idx" while_builder
      in
      let _ = L.build_store old_data1 new_box1 while_builder in
      let _ =
        L.build_store
          (L.build_add idx1 (mk_int 1) "tmp_iter_inc" while_builder)
          iterator while_builder
      in
      let () = add_terminal while_builder (L.build_br pred_bb) in
      let bool_val =
        L.build_icmp L.Icmp.Slt
          (L.build_load iterator "tmp_iter" pred_builder)
          length1 "tmp_iter_cond" pred_builder
      in
      let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in

      (* merge basic block *)
      (* building loop2 *)
      let iterator2 = L.build_alloca i32_t "alloc_iter2" merge_builder in
      let _ = L.build_store (mk_int 0) iterator2 merge_builder in

      let pred_bb2 =
        L.append_block context "while_cond2" list_concat_function
      in
      let body_bb2 =
        L.append_block context "while_body2" list_concat_function
      in
      let merge_bb2 = L.append_block context "merge2" list_concat_function in

      let while_builder2 = L.builder_at_end context body_bb2 in
      let merge_builder2 = L.builder_at_end context merge_bb2 in
      let pred_builder2 = L.builder_at_end context pred_bb2 in
      let _ = L.build_br pred_bb2 merge_builder in

      (* body basic block 2 *)
      let idx2 = L.build_load iterator2 "idx2" while_builder2 in
      let old_data2 =
        L.build_load
          (L.build_gep casted_data2 [| idx2 |] "data2" while_builder2)
          "tmp_load" while_builder2
      in
      let curr_idx =
        L.build_add idx2 length1 "new_struct_idx_data2" while_builder2
      in
      let new_box2 =
        L.build_gep new_casted_data [| curr_idx |] "tmp_set_idx" while_builder2
      in
      let _ = L.build_store old_data2 new_box2 while_builder2 in
      let _ =
        L.build_store
          (L.build_add idx2 (mk_int 1) "tmp_iter_inc" while_builder2)
          iterator2 while_builder2
      in
      let () = add_terminal while_builder2 (L.build_br pred_bb2) in
      let bool_val2 =
        L.build_icmp L.Icmp.Slt
          (L.build_load iterator2 "tmp_iter" pred_builder2)
          length2 "tmp_iter_cond" pred_builder2
      in
      let _ =
        L.build_cond_br bool_val2 body_bb2 merge_bb2 pred_builder2
        (* end merge basic block *)
      in

      let end_bb = L.append_block context "end_add" list_concat_function in
      let _ = L.build_br end_bb merge_builder2 in
      let t = L.build_ret new_casted_struct in
      let () = add_terminal (L.builder_at_end context end_bb) t in
      list_concat_function
    in
    let list_concat ty =
      match
        L.lookup_function ("_LIST_CONCAT_" ^ A.typ_to_string ty) the_module
      with
      | Some f -> f
      | None -> build_list_concat_function ty
    in

    (* LIST ADD FUNCTION BEGIN *)
    let build_list_add_function ty =
      let mk_int i = L.const_int i32_t i in
      let list_struct_type =
        L.packed_struct_type context [| i32_t; i32_t; i32_t; ltype_of_typ ty |]
      in
      let list_struct_ptr = L.pointer_type list_struct_type in
      let list_add_function =
        L.define_function
          ("_LIST_ADD_" ^ A.typ_to_string ty)
          (L.function_type list_struct_ptr
             (Array.of_list [ list_struct_ptr; ltype_of_typ ty ]))
          the_module
      in
      let list_add_function_builder =
        L.builder_at_end context (L.entry_block list_add_function)
      in
      let listt = L.param list_add_function 0 in
      let value = L.param list_add_function 1 in
      let data =
        L.build_struct_gep listt 3 "tmp_data" list_add_function_builder
      in
      let casted_data =
        L.build_bitcast data
          (L.pointer_type (ltype_of_typ ty))
          "tmp_l_source_data_cast" list_add_function_builder
      in
      let source_length =
        L.build_load
          (L.build_struct_gep listt 1 "tmp" list_add_function_builder)
          "tmp" list_add_function_builder
      in
      let new_length =
        L.build_add source_length (mk_int 1) "tmp_new_len"
          list_add_function_builder
      in
      let new_size =
        L.build_mul new_length (lval_of_type_size ty) "tmp_new_size"
          list_add_function_builder
      in
      let struct_size =
        L.build_add (mk_int 12) new_size "tmp_struct_size"
          list_add_function_builder
      in
      let struct_malloc =
        L.build_array_malloc i8_t struct_size "new_struct_malloc"
          list_add_function_builder
      in
      let new_casted_struct =
        L.build_bitcast struct_malloc
          (L.pointer_type list_struct_type)
          "tmp_l_cast" list_add_function_builder
      in
      let new_data =
        L.build_struct_gep new_casted_struct 3 "tmp_data"
          list_add_function_builder
      in
      let new_casted_data =
        L.build_bitcast new_data
          (L.pointer_type (ltype_of_typ ty))
          "tmp_l_dest_data_cast" list_add_function_builder
      in
      let iterator =
        L.build_alloca i32_t "alloc_iter" list_add_function_builder
      in
      let _ = L.build_store (mk_int 0) iterator list_add_function_builder in

      (* building loop *)
      let merge_bb = L.append_block context "merge" list_add_function in
      let pred_bb = L.append_block context "while_cond" list_add_function in
      let body_bb = L.append_block context "while_body" list_add_function in

      let while_builder = L.builder_at_end context body_bb in
      let merge_builder = L.builder_at_end context merge_bb in
      let pred_builder = L.builder_at_end context pred_bb in
      let _ = L.build_br pred_bb list_add_function_builder in

      (* body basic block *)
      let iter_val = L.build_load iterator "tmp_iter" while_builder in
      let old_data =
        L.build_load
          (L.build_gep casted_data [| iter_val |] "tmp_get_idx" while_builder)
          "tmp_load" while_builder
      in
      let new_box =
        L.build_gep new_casted_data [| iter_val |] "tmp_set_idx" while_builder
      in
      let _ = L.build_store old_data new_box while_builder in
      let _ =
        L.build_store
          (L.build_add iter_val (mk_int 1) "tmp_iter_inc" while_builder)
          iterator while_builder
      in
      (* end body basic block *)
      let () = add_terminal while_builder (L.build_br pred_bb) in
      (* pred basic block *)
      let bool_val =
        L.build_icmp L.Icmp.Slt
          (L.build_load iterator "tmp_iter" pred_builder)
          source_length "tmp_iter_cond" pred_builder
      in

      (* end pred basic block *)
      let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in

      (* merge basic block *)
      let _ =
        L.build_store value
          (L.build_gep new_casted_data [| source_length |] "tmp_set_idx"
             merge_builder)
          merge_builder
      in
      let _ =
        L.build_store
          (L.build_load
             (L.build_struct_gep listt 0 "tmp" merge_builder)
             "tmp_0_load" merge_builder)
          (L.build_struct_gep new_casted_struct 0 "tmp" merge_builder)
          merge_builder
      in
      let _ =
        L.build_store new_length
          (L.build_struct_gep new_casted_struct 1 "tmp" merge_builder)
          merge_builder
      in
      let _ =
        L.build_store
          (L.build_load
             (L.build_struct_gep listt 2 "tmp" merge_builder)
             "tmp_0_store" merge_builder)
          (L.build_struct_gep new_casted_struct 2 "tmp" merge_builder)
          merge_builder
        (* end merge basic block *)
      in
      let end_bb = L.append_block context "end_add" list_add_function in
      let _ = L.build_br end_bb merge_builder in
      let t = L.build_ret new_casted_struct in
      let () = add_terminal (L.builder_at_end context end_bb) t in
      list_add_function
    in
    let list_add ty =
      match
        L.lookup_function ("_LIST_ADD_" ^ A.typ_to_string ty) the_module
      with
      | Some f -> f
      | None -> build_list_add_function ty
    in
    (* Construct code for an expression; return its value *)
    (* NOTE: expr is guaranteed to not modify the env *)
    let rec expr builder env ((etype, e) : sexpr) =
      let rexpr = expr builder env in
      let global_str s n = L.build_global_stringptr s n builder in
      let mk_int i = L.const_int i32_t i in
      (* let ltype_of_typs ts = Array.of_list (List.map ltype_of_typ ts) in *)
      let lval_of_prim p =
        match p with
        | A.Int i -> mk_int i
        | A.Float f -> L.const_float float_t f
        | A.String s -> global_str (Scanf.unescaped s) "string"
        | A.Boolean b -> L.const_int i1_t (if b then 1 else 0)
      and type_sym t =
        match t with
        | A.INT -> mk_int 0
        | A.BOOLEAN -> mk_int 1
        | A.FLOAT -> mk_int 2
        | A.STRING -> mk_int 3
        | A.LAMBDA _ -> mk_int 4
        | A.LIST _ -> mk_int 10
        | A.TUPLE _ -> mk_int 11
        | A.TABLE _ -> mk_int 12
        | A.NONE -> mk_int 100
      and mallocate llval =
        let v = L.build_malloc (L.type_of llval) "alc_tmp" builder in
        let _ = L.build_store llval v builder in
        v
      in
      match e with
      | SIntLit i -> lval_of_prim (A.Int i)
      | SFloatLit f -> lval_of_prim (A.Float f)
      | SStringLit s -> lval_of_prim (A.String s)
      | SBoolLit b -> lval_of_prim (A.Boolean b)
      | SListLit (t, ps) ->
          let len = L.const_int i32_t (List.length ps) in
          let content =
            type_sym (A.LIST None)
            :: len :: type_sym t :: List.map lval_of_prim ps
          in
          let value = L.const_packed_struct context (Array.of_list content) in
          mallocate value
      | STupleLit (ts, ps) ->
          let len = L.const_int i32_t (List.length ps) in
          let types = List.map type_sym ts in
          let content =
            type_sym (A.TUPLE None) :: len :: (types @ List.map lval_of_prim ps)
          in
          let value = L.const_packed_struct context (Array.of_list content) in
          mallocate value
      | STableLit (ts, pss) ->
          let num_rows = L.const_int i32_t (List.length pss) in
          let row_data =
            List.map
              (fun row -> rexpr (A.TUPLE (Some ts), STupleLit (ts, row)))
              pss
          in
          let content =
            type_sym (A.TABLE None)
            :: num_rows :: type_sym (A.TUPLE None) :: row_data
          in
          let value = L.const_packed_struct context (Array.of_list content) in
          mallocate value
      | SBinop (e1, op, e2) ->
          let t, _ = e1
          and e1' = rexpr e1
          and e2' = rexpr e2
          and raise_typerr op t =
            raise
              (Failure
                 ("Internal Error: " ^ op ^ " with " ^ t
                ^ " operands not allowed"))
          in
          (match t with
          | A.INT -> (
              match op with
              | A.AND -> raise_typerr "AND" "int"
              | A.OR -> raise_typerr "OR" "int"
              | A.Add -> L.build_add
              | A.Sub -> L.build_sub
              | A.Mul -> L.build_mul
              | A.Div -> L.build_sdiv
              | A.Pow -> raise (Failure "pow for int is not impleemented")
              | A.Log ->
                  raise (Failure "log for int is not impleemented")
                  (* TODO: no operator in LLVM so maybe make this an actual function call? *)
              | A.GT -> L.build_icmp L.Icmp.Sgt
              | A.GTE -> L.build_icmp L.Icmp.Sge
              | A.LT -> L.build_icmp L.Icmp.Slt
              | A.LTE -> L.build_icmp L.Icmp.Sle
              | A.EQ -> L.build_icmp L.Icmp.Eq
              | A.NEQ -> L.build_icmp L.Icmp.Ne
              | A.Mod -> L.build_srem)
          | A.FLOAT -> (
              match op with
              | A.AND -> raise_typerr "AND" "float"
              | A.OR -> raise_typerr "OR" "float"
              | A.Add -> L.build_fadd
              | A.Sub -> L.build_fsub
              | A.Mul -> L.build_fmul
              | A.Div -> L.build_fdiv
              | A.Pow -> raise (Failure "pow for float is not impleemented")
              | A.Log ->
                  raise (Failure "log for float is not impleemented")
                  (* TODO: no operator in LLVM so maybe make this an actual function call? *)
              | A.GT -> L.build_fcmp L.Fcmp.Ogt
              | A.GTE -> L.build_fcmp L.Fcmp.Oge
              | A.LT -> L.build_fcmp L.Fcmp.Olt
              | A.LTE -> L.build_fcmp L.Fcmp.Ole
              | A.EQ -> L.build_fcmp L.Fcmp.Oeq
              | A.NEQ -> L.build_fcmp L.Fcmp.One
              | A.Mod -> L.build_frem)
          | A.BOOLEAN -> (
              match op with
              | A.AND -> L.build_and
              | A.OR -> L.build_or
              | A.EQ -> L.build_icmp L.Icmp.Eq
              | A.NEQ -> L.build_icmp L.Icmp.Ne
              | _ -> raise_typerr "ARITHMETIC OP" "bool")
          | _ -> raise_typerr "BINOP" "non-int/float/bool")
            e1' e2' "tmp" builder
      | SUnop (op, e) ->
          let t, _ = e in
          let e' = rexpr e in
          (match op with
          | A.NOT when t = A.BOOLEAN -> L.build_not
          | A.NEG when t = A.FLOAT -> L.build_fneg
          | A.NEG when t = A.INT -> L.build_neg
          | _ -> raise (Failure "Internal Error: Not a unary operator"))
            e' "tmp" builder
      (* additional logic required here to cast complex types into pointers *)
      | SVar s -> L.build_load (lookup s env) s builder
      | SIfExpr (cond, e1, e2) ->
          let cond' = rexpr cond in
          let e1' = rexpr e1 in
          let e2' = rexpr e2 in
          L.build_select cond' e1' e2' "tmp" builder
      (* lambdas are extracted and declared on a seperate pass before this already
         so here we just have to return the declared function *)
      | SLambda (n, _, _) ->
          let llval, _ = StringMap.find n lambda_decls in
          llval
      | SLamCall (n, args) ->
          let llval = rexpr (etype, SVar n) in
          let llargs = List.map (fun x -> rexpr x) args in
          let result = match etype with A.NONE -> "" | _ -> "lambda_result" in
          L.build_call llval (Array.of_list llargs) result builder
      | SCall ("length", args) ->
          let listt = rexpr (List.hd args) in
          let cast =
            L.build_bitcast listt
              (L.pointer_type
                 (L.packed_struct_type context [| i32_t; i32_t; i32_t |]))
              "tmp_l_cast" builder
          in
          L.build_load (L.build_struct_gep cast 1 "tmp" builder) "tmp" builder
      | SCall ("distance", args) ->
          let complex = rexpr (List.hd args) in
          L.build_load
            (L.build_struct_gep complex 1 "tmp" builder)
            "tmp" builder
      | SCall ("size", args) ->
          let table = rexpr (List.hd args) in
          let row_list = L.build_struct_gep table 3 "tmp_data" builder in
          let tuple =
            L.build_load
              (L.build_struct_gep row_list 0 "tmp" builder)
              "tmp" builder
          in
          let num_rows =
            L.build_load
              (L.build_struct_gep table 1 "tmp" builder)
              "tmp" builder
          in
          let num_cols =
            L.build_load
              (L.build_struct_gep tuple 1 "tmp" builder)
              "tmp" builder
          in
          let len = L.const_int i32_t 2 in
          let types = [ mk_int 0; mk_int 0 ] in
          let content =
            type_sym (A.TUPLE None) :: len :: (types @ [ num_rows; num_cols ])
          in
          let value = L.const_packed_struct context (Array.of_list content) in
          mallocate value
      | SCall ("get", args) ->
          let idx = rexpr (List.hd (List.tl args)) in
          let listt = rexpr (List.hd args) in
          let cast =
            L.build_bitcast listt
              (L.pointer_type
                 (L.packed_struct_type context
                    [| i32_t; i32_t; i32_t; ltype_of_typ etype |]))
              "tmp_l_cast" builder
          in
          let inner_list = L.build_struct_gep cast 3 "tmp_data" builder in
          let cast_inner =
            L.build_bitcast inner_list
              (L.pointer_type (ltype_of_typ etype))
              "tmp_l_data_cast" builder
          in
          L.build_load
            (L.build_gep cast_inner [| idx |] "tmp_idx" builder)
            "tmp_get_load" builder
      | SCall ("add", args) ->
          let elem_t, _ = List.hd (List.tl args) in
          let list_struct_type =
            L.packed_struct_type context
              [| i32_t; i32_t; i32_t; ltype_of_typ elem_t |]
          in
          let list_struct_ptr = L.pointer_type list_struct_type in
          let value = rexpr (List.hd (List.tl args)) in
          let listt = rexpr (List.hd args) in
          let cast =
            L.build_bitcast listt list_struct_ptr "tmp_l_cast" builder
          in
          let _ = build_list_add_function elem_t in
          let new_list =
            L.build_call (list_add elem_t)
              (Array.of_list [ cast; value ])
              "_FUNC_VAL" builder
          in
          new_list
      | SCall ("concat", args) ->
          let elem_t =
            match List.hd args with
            | A.LIST (Some t), _ -> t
            | _ -> raise (Failure "List builtin add called on type not a list")
          in
          let list_struct_type =
            L.packed_struct_type context
              [| i32_t; i32_t; i32_t; ltype_of_typ elem_t |]
          in
          let list_struct_ptr = L.pointer_type list_struct_type in
          let listt2 = rexpr (List.hd (List.tl args)) in
          let listt1 = rexpr (List.hd args) in
          let cast1 =
            L.build_bitcast listt1 list_struct_ptr "tmp_l_cast1" builder
          in
          let cast2 =
            L.build_bitcast listt2 list_struct_ptr "tmp_l_cast2" builder
          in
          let _ = build_list_concat_function elem_t in
          let new_list =
            L.build_call (list_concat elem_t)
              (Array.of_list [ cast1; cast2 ])
              "_FUNC_VAL" builder
          in
          new_list
      | SCall ("replace", args) ->
          let elem_t =
            match List.hd args with
            | A.LIST (Some t), _ -> t
            | _ -> raise (Failure "List builtin add called on type not a list")
          in
          let list_struct_type =
            L.packed_struct_type context
              [| i32_t; i32_t; i32_t; ltype_of_typ elem_t |]
          in
          let list_struct_ptr = L.pointer_type list_struct_type in
          let index = rexpr (List.hd (List.tl args)) in
          let value = rexpr (List.hd (List.tl (List.tl args))) in
          let listt = rexpr (List.hd args) in
          let cast =
            L.build_bitcast listt list_struct_ptr "tmp_l_cast1" builder
          in
          let inner_list = L.build_struct_gep cast 3 "tmp_data" builder in
          let cast_inner =
            L.build_bitcast inner_list
              (L.pointer_type (ltype_of_typ elem_t))
              "tmp_l_data_cast" builder
          in
          let box = L.build_gep cast_inner [| index |] "tmp_set_idx" builder in
          let _ = L.build_store value box builder in
          cast
      | SCall ("zeros", args) ->
          let int_val =
            match List.hd (List.tl args) with
            | _, SIntLit i -> i
            | _ -> raise (Failure "Only numbers are checked")
          in
          let data = List.init int_val (fun _ -> lval_of_prim (A.Int 0) ) in
          let len = L.const_int i32_t int_val in
          let content =
            type_sym (A.LIST None) :: len :: type_sym A.INT :: data
          in
          let value = L.const_packed_struct context (Array.of_list content) in
          mallocate value
      | SCall ("table_get", args) ->
          let id1 = rexpr (List.nth args 1) in
          let id2 = rexpr (List.nth args 2) in
          let table = rexpr (List.hd args) in
          let table_list = L.build_struct_gep table 3 "tmp_data" builder in
          let table_tuple =
            L.build_load
              (L.build_gep table_list [| id1 |] "tmp" builder)
              "tmp" builder
          in
          let table_tuple_data =
            L.build_struct_gep table_tuple 4 "tmp_data" builder
          in
          L.build_load
            (L.build_gep table_tuple_data [| id2 |] "tmp" builder)
            "tmp" builder
      | SCall ("get_row", args) ->
          let id1 = rexpr (List.nth args 1) in
          let table = rexpr (List.hd args) in
          let table_list = L.build_struct_gep table 3 "tmp_data" builder in
          L.build_load
            (L.build_gep table_list [| id1 |] "tmp" builder)
            "tmp" builder
      | SCall ("get_col", args) ->
          let id1 = rexpr (List.nth args 1) in
          let table = rexpr (List.hd args) in
          let len =
            L.build_load
              (L.build_struct_gep table 1 "tmp" builder)
              "tmp" builder
          in
          let len_int =
            match L.int64_of_const len with
            | Some i -> Int64.to_int i
            | None -> raise (Failure "Length should be integer element")
          in
          let table_list = L.build_struct_gep table 3 "tmp_data" builder in
          let first_row =
            L.build_load
              (L.build_gep table_list [| mk_int 0 |] "tmp" builder)
              "tmp" builder
          in
          let tuple_types = L.build_struct_gep first_row 3 "tmp_data" builder in
          let list_type =
            L.build_load
              (L.build_gep tuple_types [| id1 |] "tmp" builder)
              "tmp" builder
          in
          let get_list_elem i =
            let tuple =
              L.build_load
                (L.build_gep table_list [| mk_int i |] "tmp" builder)
                "tmp" builder
            in
            let tuple_data = L.build_struct_gep tuple 4 "tmp_data" builder in
            L.build_load
              (L.build_gep tuple_data [| id1 |] "tmp" builder)
              "tmp" builder
          in
          let col_data = List.init len_int get_list_elem in
          let content =
            type_sym (A.LIST None) :: len :: list_type :: col_data
          in
          let value = L.const_packed_struct context (Array.of_list content) in
          mallocate value
      | SCall (f, args) ->
          let cast_complex (t, sx) =
            let v = rexpr (t, sx) in
            match t with
            | A.LIST _ ->
                L.build_bitcast v (L.pointer_type i8_t) "var_list_tmp" builder
            | A.TUPLE _ ->
                L.build_bitcast v (L.pointer_type i8_t) "var_tuple_tmp" builder
            | A.TABLE _ ->
                L.build_bitcast v (L.pointer_type i8_t) "var_table_tmp" builder
            | _ -> v
          in
          let llargs = List.map cast_complex args in
          let userdef dom =
            let argtypes = List.map (fun (t, _) -> t) args in
            let match_args decls =
              List.find
                (fun decl ->
                  List.map (fun (t, _) -> t) decl.sformals = argtypes)
                decls
            in
            let fdef, fdecl =
              try StringMap.find f dom
              with Not_found -> (
                try
                  StringMap.find
                    (match_args (StringMap.find f overload_decls)).sfname dom
                with Not_found ->
                  raise (Failure (f ^ " is not a declared function")))
            in
            let result =
              match fdecl.styp with A.NONE -> "" | _ -> f ^ "_result"
            in
            L.build_call fdef (Array.of_list llargs) result builder
          and predef f =
            let pdecl, rt =
              try StringMap.find f predef_decls
              with Not_found ->
                raise (Failure (f ^ " is not a recognized built-in function"))
            in
            let result = match rt with A.NONE -> "" | _ -> f ^ "_result" in
            L.build_call pdecl (Array.of_list llargs) result builder
          (* and is_lambda = StringMap.mem f env *)
          and is_predef = List.mem f P.predef_names in
          (* if is_lambda then userdef lambda_decls else *)
          if is_predef then predef f else userdef function_decls
      | SNoExp -> L.const_null void_t
    in
    (* return a builder env tuple *)
    let rec stmt builder env iters = function
      | SBlock sl ->
          let _, b, _ =
            List.fold_left
              (fun (e, b, i) s -> stmt b e i s)
              (env, builder, iters) sl
          in
          (env, b, iters)
      | SWhile (cond, s) ->
          (* some work for scoping *)
          let merge_bb = L.append_block context "merge" the_function in
          let pred_bb = L.append_block context "while_cond" the_function in
          let _ = L.build_br pred_bb builder in
          let body_bb = L.append_block context "while_body" the_function in
          let _, while_builder, _ =
            stmt (L.builder_at_end context body_bb) env (merge_bb :: iters) s
          in
          let () = add_terminal while_builder (L.build_br pred_bb) in
          let pred_builder = L.builder_at_end context pred_bb in
          let bool_val = expr pred_builder env cond in
          let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
          (env, L.builder_at_end context merge_bb, iters)
      | SIf (cond, s1, s2) ->
          let bool_val = expr builder env cond in
          let merge_bb = L.append_block context "merge" the_function in
          let branch_instr = L.build_br merge_bb in
          let then_bb = L.append_block context "then" the_function in
          let _, then_builder, _ =
            stmt (L.builder_at_end context then_bb) env iters s1
          in
          let () = add_terminal then_builder branch_instr in
          let else_bb = L.append_block context "else" the_function in
          let _, else_builder, _ =
            stmt (L.builder_at_end context else_bb) env iters s2
          in
          let () = add_terminal else_builder branch_instr in
          let _ = L.build_cond_br bool_val then_bb else_bb builder in
          (env, L.builder_at_end context merge_bb, iters)
      | SReturn e ->
          let cast_complex (t, sx) =
            let v = expr builder env (t, sx) in
            match t with
            | A.LIST _ ->
                L.build_bitcast v (L.pointer_type i8_t) "var_list_tmp" builder
            | A.TUPLE _ ->
                L.build_bitcast v (L.pointer_type i8_t) "var_tuple_tmp" builder
            | A.TABLE _ ->
                L.build_bitcast v (L.pointer_type i8_t) "var_table_tmp" builder
            | _ -> v
          in
          let _ =
            match fdecl.styp with
            | A.NONE -> L.build_ret_void builder
            | _ -> L.build_ret (cast_complex e) builder
          in
          (env, builder, iters)
      | SBreak -> (
          match iters with
          | [] -> raise (Failure "Cannot call break outside of iterative loop")
          | i :: is ->
              let _ = L.build_br i builder in
              (env, builder, is))
      | SDeclare (t, n, e) ->
          let e' = expr builder env e in
          let v = L.build_alloca (L.type_of e') n builder in
          let env' = StringMap.add n (t, v) env in
          let _ = L.build_store e' v builder in
          (env', builder, iters)
      | SAssign (n, e) ->
          let e' = expr builder env e in
          let _ = L.build_store e' (lookup n env) builder in
          (env, builder, iters)
      | SExpr e ->
          let _ = expr builder env e in
          (env, builder, iters)
    in

    let _, builder, _ = stmt builder formal_env [] (SBlock fdecl.sbody) in
    add_terminal builder
      (match fdecl.styp with
      | A.NONE -> L.build_ret_void
      | A.FLOAT -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in
  let lambda_functions =
    List.map (fun (_, (_, fd)) -> fd) (StringMap.bindings lambda_decls)
  in
  List.iter build_function_body (program @ lambda_functions);
  the_module
