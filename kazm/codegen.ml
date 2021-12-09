module A = Ast
module L = Llvm
open Sast

module SMap = Map.Make(String)

let log_to_file str=
ignore()

let gen (sglobals, sfunction_decls) =
  (* Set up module & context *)
  let context = L.global_context () in
  let m = L.create_module context "kazm" in

  (* Set up types in the context *)
  let i1_t = L.i1_type context in
  let i8_t = L.i8_type context in
  let i32_t = L.i32_type context in
  let double_t = L.double_type context in
  let char_t = i8_t in
  let void_t = L.void_type context in
  let char_ptr_t = L.pointer_type char_t in

  (* Map our AST type to LLVM type *)
  let typ_to_t = function
      A.Void -> void_t
    | A.Bool -> i1_t
    | A.Int -> i32_t
    | A.Double -> double_t
  in

  let codegen_func_decl name ret_t arg_ts =
    let helper name arg_ts = match name with 
      | "print" -> (Array.of_list [char_ptr_t])
      | "println" -> (Array.of_list [char_ptr_t]) 
      | "int_print" -> (Array.of_list [i32_t])
      | "int_println" -> (Array.of_list [i32_t])
      | "double_print" -> (Array.of_list [double_t])
      | "double_println" -> (Array.of_list [double_t])
      | "next_int" -> (Array.of_list [])
      | _ -> (Array.of_list (List.map typ_to_t arg_ts))
    in
    let func_t = 
      L.function_type ret_t (helper name arg_ts) 
    in 
    L.declare_function name func_t m
  in

  let add_func_decl map name ret_t arg_ts =
    SMap.add name (codegen_func_decl name ret_t arg_ts) map
  in

  let codegen_func_def name ret_t arg_ts =
    let func_t = L.function_type ret_t (Array.of_list arg_ts) in
      L.define_function name func_t m
  in

  let add_func_def map name ret_t arg_ts =
    SMap.add name (codegen_func_def name ret_t arg_ts) map
  in

  let all_funcs = SMap.empty in
  (* Builtins... *)
  (* we have hard coded the arg_ts for now, modify when character types are properly supported *)
  let all_funcs = add_func_decl all_funcs "print" void_t [] in
  let all_funcs = add_func_decl all_funcs "println" void_t [] in
  let all_funcs = add_func_decl all_funcs "int_print" void_t [] in
  let all_funcs = add_func_decl all_funcs "int_println" void_t [] in
  let all_funcs = add_func_decl all_funcs "double_print" void_t [] in
  let all_funcs = add_func_decl all_funcs "double_println" void_t [] in
  let all_funcs = add_func_decl all_funcs "next_int" i32_t [] in

  (* Codegen function definitions *)
  let codegen_func_sig all_funcs func =
    add_func_def all_funcs func.sfname (typ_to_t func.styp) [] (* func.sformals *)
  in

  (* let A.PFuncs(funcs) = prog in *)
  (* let all_funcs = List.fold_left codegen_func_sig all_funcs funcs in *)
  let all_funcs = List.fold_left codegen_func_sig all_funcs sfunction_decls in

  (* Set up tables for globals, locals, and function parameters *)
  let locals_tbl = SMap.empty in 
  let params_tbl = SMap.empty in
  let globals_tbl = SMap.empty in 

  (* lookup will be used within a function *)
  let lookup name = 
    try SMap.find name locals_tbl with 
      | Not_found -> 
        try SMap.find name params_tbl with 
        | Not_found ->
          try SMap.find name globals_tbl with 
          | Not_found -> raise (Failure("Unknown variable " ^ name))
  in

  (* Codegen for an expression *)
  let rec codegen_expr builder ((typ, e) : sexpr) = match e with
    (* Function call *)
      SCall(cname, exprs) ->
        (* let arg_str = L.build_global_stringptr carg "arg" builder in *)
        let arg_array = Array.of_list (List.map (codegen_expr builder) exprs) in
        (* todo: need to make sure these functions actually exist, etc *)
        L.build_call (SMap.find cname all_funcs) arg_array "" builder
    (* New bool literal *)
    | SBoolLit(value) -> L.const_int i1_t (if value then 1 else 0)
    (* New 32-bit integer literal *)
    | SLiteral(value) -> L.const_int i32_t value
    | SDliteral(value) -> L.const_float double_t (float_of_string value)
    (* New string literal (just make a new global string) *)
    | SStringLit(value) -> L.build_global_stringptr value "globalstring" builder
    (* Handle new variables; s is the name of the variable *)
    | SId (s) -> L.build_load (lookup s) s builder 
    (* Assign expression e to a new bind(type, name) *)
    (* s is the name of the lhs variable that receives the assignment, value the expr on the rhs *)
    | SAssign(s, value) -> let e' = codegen_expr builder value in
                           let lh = L.build_alloca (typ_to_t typ) s builder in
                           ignore(L.build_store e' lh builder); e'
    | SBinop(e1, op, e2) ->
      (* Lookup right thing to build in llvm *)
      let lbuild = match op with
          A.Add -> L.build_add
        | A.Sub -> L.build_sub
        | A.Mult -> L.build_mul
        | A.Div -> L.build_sdiv
        | A.Mod -> L.build_srem
        | A.Equal -> L.build_icmp L.Icmp.Eq
        | A.Neq -> L.build_icmp L.Icmp.Ne
        | A.Less -> L.build_icmp L.Icmp.Slt
        | A.Leq -> L.build_icmp L.Icmp.Sle
        | A.Greater -> L.build_icmp L.Icmp.Sgt
        | A.Geq -> L.build_icmp L.Icmp.Sge
      in
      lbuild (codegen_expr builder e1) (codegen_expr builder e2) "im" builder
  in



  (* Generate globals *)
  let codegen_globals globals = 
    let add_to_globals_tbl tbl (t, n) = 
      let init = L.const_int (typ_to_t t) 0 in 
      SMap.add n (L.define_global n init m) tbl
    in List.map (add_to_globals_tbl globals_tbl) globals
  in 

  (* Generate locals *)
  (* let codegen_locals formals locals builder fn = 
    let add_to_params_tbl tbl (t, n) v = (* type name value *)
      L.set_value_name n v;
      let formal = L.build_alloca (typ_to_t t) n builder in 
        L.build_store v formal builder; 
      SMap.add n formal tbl 
    and add_to_locals_tbl tbl (t, n) = 
      let local = L.build_alloca (typ_to_t t) n builder in
      SMap.add n local tbl 
    in 
    List.fold_left2 add_to_params_tbl params_tbl formals (Array.to_list (L.params fn));
    (* ignore(List.map2 (add_to_params_tbl params_tbl) formals (Array.to_list (L.params fn))); *)
    List.map (add_to_locals_tbl locals_tbl) locals
  in  *)

  (* Codegen for function body *)
  let gen_func func =
    (* let A.Func(bind, body) = func in *)
    (* let A.Bind(typ, name) = bind in *) 
    let body =  func.sbody in
    let typ = func.styp in 
    let name = func.sfname in 
    let formals = func.sformals in
    let locals = func.slocals in 
    (* Defines the func *)
    let fn = SMap.find name all_funcs in
    (* Clear locals_tbl and params_tbl *)
    let locals_tbl = SMap.empty in 
    let params_tbl = SMap.empty in 
    
    (* Codegen for a statement *)
    (* Takes builder and statement and returns a builder *)
    let rec codegen_stmt builder = function
      (* For expressions we just codegen the expression *)
        SExpr(e) -> codegen_expr builder e; builder
      (* For a block of statements, just fold *)
      | SBlock(es) -> List.fold_left codegen_stmt builder es
      | SReturnVoid -> ignore (L.build_ret_void builder); builder
      | SReturn(expr) -> ignore (L.build_ret (codegen_expr builder expr) builder); builder
      (* If-statements *)
      | SIf(cond, true_stmts, false_stmts) ->
        (* Codegen the condition evaluation *)
        let gcond = codegen_expr builder cond in
        (* Generate the block that we come back to after both branches *)
        let join_blk = L.append_block context "join" fn in
        let join_builder = L.builder_at_end context join_blk in

        (* This function takes a builder and builds a `br` instruction, which
          (br)anches back to the start of the join_blk block made above *)
        let build_join = L.build_br join_blk in

        (* True branch *)
        (* Add the block we go to if we take this branch (cond is true) *)
        let true_blk = L.append_block context "take" fn in
        let true_builder = L.builder_at_end context true_blk in
        (* Build this branch's statements into this block *)
        codegen_stmt true_builder true_stmts;
        build_join true_builder;

        (* False branch *)
        (* Add the block we go to if we don't take this branch (cond is false) *)
        let false_blk = L.append_block context "dont_take" fn in
        let false_builder = L.builder_at_end context false_blk in
        codegen_stmt false_builder false_stmts;
        build_join false_builder;

        (* Build the actual conditional branch *)
        ignore (L.build_cond_br gcond true_blk false_blk builder);
        (* Finally return the new builder at end of merge *)
        join_builder
      (* While-statements *)
      | SWhile(cond, stmts) ->
        (* Start at the start block and its builder *)
        let start_blk = L.append_block context "start" fn in
        let start_builder = L.builder_at_end context start_blk in

        (* Loop block that we keep repeating *)
        let loop_blk = L.append_block context "loop" fn in
        let loop_builder = L.builder_at_end context loop_blk in
        (* Loop body (an iteration) *)
        codegen_stmt loop_builder stmts;
        (* Back to start after a loop iteration *)
        L.build_br start_blk loop_builder;

        (* Generate the end block where we end up after the while cond becomes false *)
        let end_blk = L.append_block context "end" fn in
        let end_builder = L.builder_at_end context end_blk in

        (* Codegen the condition evaluation *)
        let gcond = codegen_expr start_builder cond in
        (* Build the branch instr *)
        ignore (L.build_cond_br gcond loop_blk end_blk start_builder);

        (* Branch to start *)
        ignore (L.build_br start_blk builder);
      
        (* Continue building after the end of the loop *)
        end_builder

    in    




    (* Build all statements *)
    let fn_builder = L.builder_at_end context (L.entry_block fn) in


    let rec zipWith3
    (f : 'a -> 'b -> 'c -> 'd)
    (l1 : 'a list)
    (l2 : 'b list)
    (l3 : 'c list) : 'd list
    = match l1, l2, l3 with 
    | [], _, _ -> []
    | _, [], _ -> []
    | _, _, [] -> []
    | h1::t1, h2::t2, h3::t3 -> 
      let r = f h1 h2 h3 in
      r :: zipWith3 f t1 t2 t3 
    in 

    (* Move local generation here *)
    let codegen_locals local_vars = 
      let add_formal table (the_type, name, value) = 
      (* let add_formal table (the_type,name) value = *)
        L.set_value_name name value;
        let local = L.build_alloca (typ_to_t the_type) name fn_builder in 
        SMap.add name local table
      in
      let add_local table (the_type,name) =
        let t = typ_to_t the_type
        in
        let local_var = L.build_alloca t name fn_builder
        in SMap.add name local_var table
      (* in *)
      (* let _ = List.map2 (add_formal params_tbl) formals (Array.to_list (L.params fn))  *)
      in 
      let aux_formals = List.split formals in
      let values = Array.to_list (L.params fn) in 
      let concatenated_formals = zipWith3 (fun x y z -> (x,y,z)) (fst aux_formals) (snd aux_formals) values in 
      ignore(List.map (add_formal params_tbl) concatenated_formals); 
      (* ignore(List.map2 (add_formal params_tbl) formals (Array.to_list (L.params fn))); *)
      (* in *) (* formals is bind list where bind is typ * string *)
      ignore(List.map (add_local locals_tbl) local_vars);

    in codegen_locals locals;

    (* Generate locals *)
    (* let _ = codegen_locals formals locals fn_builder fn in *)
    (* ignore(codegen_locals formals locals fn_builder fn); *)
    codegen_stmt fn_builder (SBlock body)
  in

  (* Generate globals *)
  let _ = codegen_globals sglobals in

  let funcs = sfunction_decls in
    ignore (List.map gen_func funcs);
  m