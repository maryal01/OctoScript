module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

(* Change to MicroC, statement lists instead of globals *)
let translate (functions, statements) = 
  let context    = L.global_context () in
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context  (* char *)
  and i1_t       = L.i1_type     context  (* bool *)
  and float_t    = L.double_type context
  and void_t     = L.void_type   context

  and the_module = L.create_module context "OctoScript" in

  (* COMPLEX TYPES HMMMMMMMMM? *)
  (* IDEAS: seperate type representation in sast with prim vs complex, for easier
            pattern matching (also metadata like how long is string) *)
  (* Complex types and their pointers will have to be generated dynamically *)
  let ltype_of_typ = function
    A.INT     -> i32_t
  | A.BOOLEAN -> i1_t
  | A.FLOAT   -> float_t
  | A.NONE    -> void_t
  | A.STRING  -> L.pointer_type i8_t (* would this work?? for all? how about arrays? *)
  | A.LAMBDA  -> raise(Failure("lambda lit type is not impleemented"))
  | A.TABLE   -> raise(Failure("table lit type is not impleemented"))
  | A.TUPLE   -> raise(Failure("tuple lit type is not impleemented"))
  | A.LIST    -> raise(Failure("list lit type is not impleemented"))

  in
  (* skipping globals part because we support more than globals *)
  let printf_t : L.lltype = 
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
    L.declare_function "printf" printf_t the_module in

  let debug_func : L.llvalue = 
    L.declare_function "debug" (L.function_type i32_t [| |]) the_module in

  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = 
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty ({ styp = A.NONE; sfname = "main"; sformals = []; sbody = statements } :: functions) in
    (* TODO: code to gather the statement list into a "main" function *)
    (* maybe we still need globals????? think about scoping when done like this *)
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = (try StringMap.find fdecl.sfname function_decls with Not_found -> raise (Failure "build body func")) in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    (* TODO: not sure why we need these things
    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in *)
    let string_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
    declared variables.  Allocate each on the stack, initialize their
    value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = 
        let () = L.set_value_name n p in
          let local = L.build_alloca (ltype_of_typ t) n builder in
            let _  = L.build_store p local builder in
              StringMap.add n local m 
      in
      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
          formals
          (* NOTE: we removed the part where slocal is added *)
      in

      (* Return the value for a variable or formal argument. First check
        * locals, then globals *)


      (* Original code: 
      let lookup n = try StringMap.find n local_vars
                      with Not_found -> StringMap.find n global_vars *)
      let lookup n = try StringMap.find n local_vars with Not_found -> raise (Failure ("lookup for " ^ n ^ " failed"))
      in

      (* Construct code for an expression; return its value *)
      let rec expr builder ((_, e) : sexpr) = (match e with 
          SIntLit i -> L.const_int i32_t i
        | SFloatLit f -> L.const_float float_t f
        | SStringLit s -> L.build_global_stringptr s "string" builder
        | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
        | SListLit (_, _) -> raise(Failure("list lit is not impleemented"))
        | STupleLit (_, _) -> raise(Failure("tuple lit is not impleemented"))
        | STableLit (_, _) -> raise(Failure("table lit is not impleemented"))
        | SBinop (e1, op, e2) -> 
            let (t, _) = e1
              and e1' = expr builder e1
              and e2' = expr builder e2
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
            let e' = expr builder e in
            (match op with 
                A.NOT when t = A.BOOLEAN -> L.build_not
              | A.NEG when t = A.FLOAT -> L.build_fneg
              | A.NEG when t = A.INT -> L.build_neg
              | _ -> raise (Failure "Internal Error: Not a unary operator")
            ) e' "tmp" builder
        | SVar s -> L.build_load (lookup s) s builder
        | SIfExpr (cond, e1, e2) -> 
            let cond' = expr builder cond in
            let e1' = expr builder e1 in
            let e2' = expr builder e2 in
            L.build_select cond' e1' e2' "tmp" builder 
        | SLambda (_, _) -> raise(Failure("Lambda has not been implemented"))
        | SApply (e, f, es) ->
            let (fdef, fdecl) = (try StringMap.find f function_decls with Not_found -> raise (Failure "not found in SApply")) in
            let args = e :: es in
            let llargs = List.rev (List.map (expr builder) (List.rev args)) in
            let result = (match fdecl.styp with
                            A.NONE -> ""
                          | _ -> f ^ "_result") in
              L.build_call fdef (Array.of_list llargs) result builder
        | SCall ("print", [e]) -> L.build_call printf_func [| string_format_str ; expr builder e |] "print" builder
        | SCall ("debug", []) -> L.build_call debug_func [||] "debug" builder
        | SCall (f, args) -> 
            let (fdef, fdecl) = (try StringMap.find f function_decls with Not_found -> raise (Failure "not found in SCall"))  in
            let llargs = List.rev (List.map (expr builder) (List.rev args)) in
            let result = (match fdecl.styp with
                            A.NONE -> ""
                          | _ -> f ^ "_result") in
              L.build_call fdef (Array.of_list llargs) result builder
        | SNoExp -> L.const_null void_t)   (* Actually not quite sure? *)
      in
      
      let add_terminal builder instr = 
        (match L.block_terminator (L.insertion_block builder) with
           Some _ -> ()
         | None -> ignore (instr builder) )
      in

      let rec stmt builder = function
          SBlock sl -> List.fold_left stmt builder sl
        | SWhile (cond, s) -> 
            let pred_bb = L.append_block context "while_cond" the_function in
              let _ = L.build_br pred_bb builder in
            let body_bb = L.append_block context "while_body" the_function in
              let while_builder = stmt (L.builder_at_end context body_bb) s in
              let () = add_terminal while_builder (L.build_br pred_bb) in
            let pred_builder = L.builder_at_end context pred_bb in
	          let bool_val = expr pred_builder cond in
            let merge_bb = L.append_block context "merge" the_function in
	          let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
	          L.builder_at_end context merge_bb
        | SIf (cond, s1, s2) ->
            let bool_val = expr builder cond in
            let merge_bb = L.append_block context "merge" the_function in
              let branch_instr = L.build_br merge_bb in
            let then_bb = L.append_block context "then" the_function in
              let then_builder = stmt (L.builder_at_end context then_bb) s1 in
              let () = add_terminal then_builder branch_instr in
            let else_bb = L.append_block context "else" the_function in
              let else_builder = stmt (L.builder_at_end context else_bb) s2 in
              let () = add_terminal else_builder branch_instr in
            let _ = L.build_cond_br bool_val then_bb else_bb builder in
            L.builder_at_end context merge_bb
        | SReturn e -> 
            let _ = 
              (match fdecl.styp with
                  A.NONE -> L.build_ret_void builder 
                | _ -> L.build_ret (expr builder e) builder)
            in builder
        | SBreak -> raise(Failure("break is not impleemented"))
        | SDeclare (_, _, _) -> raise(Failure("declare is not impleemented"))
        | SAssign (n, e) ->
            let e' = expr builder e in
            let _ = L.build_store e' (lookup n) builder 
            in builder 
        | SExpr e -> let _ = expr builder e in builder 
      in
      
      let builder = stmt builder (SBlock fdecl.sbody) in
      add_terminal builder (match fdecl.styp with
          A.NONE -> L.build_ret_void
        | A.FLOAT -> L.build_ret (L.const_float float_t 0.0)
        | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
    in
    (* List.iter build_function_body functions; *)
    List.iter build_function_body ({ styp = A.NONE; sfname = "main"; sformals = []; sbody = statements } :: []);
    the_module

