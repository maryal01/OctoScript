module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

(* Change to MicroC, statement lists instead of globals *)
let translate (statements, functions) = 
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
  | A.LAMBDA  -> 
  | A.TABLE   -> 
  | A.TUPLE   -> 
  | A.LIST    -> 

  in
  (* skipping globals part because we support more than globals *)

  let print_t : L.lltype = 
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let print_func : L.llvalue = 
    L.declare_function "print" print_t the_module in

  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = 
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
    (* TODO: code to gather the statement list into a "main" function *)
    (* maybe we still need globals????? think about scoping when done like this *)
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    (* TODO: not sure why we need these things *)
    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in

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

      (* Allocate space for any locally declared variables and add the
        * resulting registers to our map *)
      let add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n local_var m 
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
          formals
          (* NOTE: we removed the part where slocal is added *)
      in

      (* Return the value for a variable or formal argument. First check
        * locals, then globals *)
      let lookup n = try StringMap.find n local_vars
                      with Not_found -> StringMap.find n global_vars
      in

      (* Construct code for an expression; return its value *)
      let rec expr builder ((_, e) : sexpr) = (match e with 
        SPrimLit p ->  
          (match p with 
              Int i     -> L.const_int i32_t i
            | String s  -> (* get string length directly in ocaml type *)
            | Float f   -> L.const_float_of_string float_t f
            | Boolean b -> L.const_int i1_t (if b then 1 else 0))
        | SListLit (t, ps) -> 
        | STupleLit (ts, ps) -> 
        | SBinop (e1, bop, e2) -> 
            let (t, _) = e1
              and e1' = expr builder e1
              and e2' = expr builder e2
              and raise_typerr op t = raise (Failure ("Internal error: " ^ op ^ " with " ^ t ^ " operands not allowed"))
            in
            (match t with 
                A.INT -> 
                  (match op with
                      A.AND -> raise_typerr "AND" "int"
                    | A.OR ->  raisee_typerr "OR" "int"
                    | A.Add -> L.build_add
                    | A.Sub -> L.build_sub
                    | A.Mul -> L.build_mul
                    | A.Div -> L.build_sdiv
                    | A.Pow -> 
                    | A.Log -> (* TODO: no operator in LLVM so maybe make this an actual function call? *)
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
                  | A.OR ->  raisee_typerr "OR" "float"
                  | A.Add -> L.build_fadd
                  | A.Sub -> L.build_fsub
                  | A.Mul -> L.build_fmul
                  | A.Div -> L.build_fdiv
                  | A.Pow -> 
                  | A.Log -> (* TODO: no operator in LLVM so maybe make this an actual function call? *)
                  | A.GT -> L.build_fcmp L.Fcmp.Sgt
                  | A.GTE -> L.build_fcmp L.Fcmp.Sge
                  | A.LT -> L.build_fcmp L.Fcmp.Slt
                  | A.LTE -> L.build_fcmp L.Fcmp.Sle
                  | A.EQ -> L.build_fcmp L.Fcmp.Eq
                  | A.NEQ -> L.build_fcmp L.Fcmp.Ne
                  | A.Mod -> L.build_frem)
              | A.BOOLEAN -> 
                (match op with
                    A.AND -> L.build_and
                  | A.OR ->  L.build_or
                  | A.EQ -> L.build_icmp L.Icmp.Eq
                  | A.NEQ -> L.build_icmp L.Icmp.Ne
                  | _ -> raise_typerr "ARITHMETIC OP" "bool")
              | _ => raise_typerr "BINOP" "non-int/float/bool"
          ) e1' e2' "tmp" builder
        | SUnop(op, e) -> 
            let (t, _) = e in
            let e' = expr builder e in
            (match op with 
                A.NOT when t = A.Bool -> L.build_not
              | A.NEG when t = A.Float -> L.build_fneg
              | A.NEG when t = A.Int -> L.build_neg
            ) e' "tmp" builder
        | SVar -> L.build_load (lookup s) s builder
        | SIfExpr (cond, e1, e2) -> 
            let cond' = expr builder cond in
            let e1' = expr builder e1 in
            let e2' = expr builder e2 in
            L.build_select cond' e1' e2' "tmp" builder 
        | SLambda -> 
        | SApply (e, f, es) ->
            let (fdef, fdecl) = StringMap.find f function_decls in
            let args = e :: es in
            let llargs = List.rev (List.map (expr builder) (List.rev args)) in
            let result = (match fdecl.styp with
                            A.Void -> ""
                          | _ -> f ^ "_result") in
              L.build_call fdef (Array.of_list llargs) result builder
        | SCall ("Print", [e]) -> L.build_call print_func [| e |] "Print" builder
        | SCall (f, args) -> 
            let (fdef, fdecl) = StringMap.find f function_decls in
            let llargs = List.rev (List.map (expr builder) (List.rev args)) in
            let result = (match fdecl.styp with
                            A.Void -> ""
                          | _ -> f ^ "_result") in
              L.build_call fdef (Array.of_list llargs) result builder
        | SNoExp -> ignore (instr builder))   (* Actually not quite sure? *)
      in
      
      let add_terminal builder instr = 
        (match L.block_terminator (L.insertion_block builder) with
           Some _ -> ()
         | None -> ignore (instr builder) )
      in

      

      let rec stmt builder = function
          SWhile -> 
        | SIf -> 
        | SReturn e -> 
            let _ = 
              (match fdecl.styp with
                  A.Void -> L.build_ret_void builder 
                | _ -> L.build_ret (expr builder e) builder)
            in builder
        | SBreak ->
        | SDeclare -> 
        | SAssign (n, e) -> 
            let e' = expr builder e in
            let _ = L.build_store e' (lookup n) builder 
            in builder 
        | SPrint -> (* TODO: Handle print as a call to a void function *)
        | SExpr e -> let _ = expr builder e in builder 

      