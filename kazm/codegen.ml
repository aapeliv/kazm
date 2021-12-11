module A = Ast
module L = Llvm
open Sast

module SMap = Map.Make(String)

let gen (bind_list, sfunction_decls) =
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

  let codegen_func_decl name ret_t arg_ts =
    let func_t = L.function_type ret_t (Array.of_list arg_ts) in
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

  (* Map our AST type to LLVM type *)
  let typ_to_t = function
      A.Void -> void_t
    | A.Bool -> i1_t
    | A.Int -> i32_t
    | A.Double -> double_t
  in

  (* Given a builder and our type, build a dummy return (e.g. if there's a missing return) *)
  let build_default_return typ ctx =
    let (builder, sp) = ctx in
    match typ with
      A.Void -> ignore (L.build_ret_void builder); ctx
    | A.Bool -> ignore (L.build_ret (L.const_int i1_t 0) builder); ctx
    | A.Int -> ignore (L.build_ret (L.const_int i32_t 0) builder); ctx
    | A.Double -> ignore (L.build_ret (L.const_float double_t 0.) builder); ctx
  in

  let all_funcs = SMap.empty in
  (* Builtins... *)
  let all_funcs = add_func_decl all_funcs "print" void_t [char_ptr_t] in
  let all_funcs = add_func_decl all_funcs "println" void_t [char_ptr_t] in
  let all_funcs = add_func_decl all_funcs "int_print" void_t [i32_t] in
  let all_funcs = add_func_decl all_funcs "int_println" void_t [i32_t] in
  let all_funcs = add_func_decl all_funcs "double_print" void_t [double_t] in
  let all_funcs = add_func_decl all_funcs "double_println" void_t [double_t] in
  let all_funcs = add_func_decl all_funcs "next_int" i32_t [] in

  (* Codegen function definitions *)
  let codegen_func_sig all_funcs func =
    (* let A.Func(bind, _) = func in *)
    (* let A.Bind(typ, name) = bind in *)
    let arg_types = List.map (fun sformal -> typ_to_t (fst sformal)) func.sformals in
    add_func_def all_funcs func.sfname (typ_to_t func.styp) arg_types
  in

  (* let A.PFuncs(funcs) = prog in *)
  (* let all_funcs = List.fold_left codegen_func_sig all_funcs funcs in *)
  let all_funcs = List.fold_left codegen_func_sig all_funcs sfunction_decls in

  (* Codegen for an expression *)
  let rec codegen_expr ctx ((typ, e) : sexpr) =
    let (builder, sp) = ctx in
    match e with
    (* Function call *)
      SCall(cname, exprs) ->
        let accum old_ctx expr =
          let (new_ctx, gend_expr) = codegen_expr old_ctx expr in
          (new_ctx, gend_expr)
        in
        let (new_ctx, args_evaluted) = Future.fold_left_map accum ctx exprs in
        (* let arg_str = L.build_global_stringptr carg "arg" builder in *)
        let arg_array = Array.of_list args_evaluted in
        (* todo: need to make sure these functions actually exist, etc *)
        let ex = L.build_call (SMap.find cname all_funcs) arg_array "" builder in
        (new_ctx, ex)
    (* New bool literal *)
    | SBoolLit(value) -> (ctx, L.const_int i1_t (if value then 1 else 0))
    (* New 32-bit integer literal *)
    | SLiteral(value) -> (ctx, L.const_int i32_t value)
    | SDliteral(value) -> (ctx, L.const_float double_t (float_of_string value))
    (* New string literal (just make a new global string) *)
    | SStringLit(value) -> (ctx, L.build_global_stringptr value "globalstring" builder)
    (* Assign expression e to a new bind(type, name) *)
    | SAssign(s, value) -> let (new_ctx, e') = codegen_expr ctx value in
                           let lh = L.build_alloca (typ_to_t typ) s builder in
                           (* TODO *)
                           ignore (L.build_store e' lh builder); (ctx, e')
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
      let (ctx1, first) = codegen_expr ctx e1 in
      let (ctx2, second) = (codegen_expr ctx1 e2) in
      let (builder, sp) = ctx2 in
      let new_expr = lbuild first second "im" builder in
      (ctx2, new_expr)
    | _ -> raise (Failure ("sast cannot be matched"))
  in

  (* Add terminator to end of a basic block *)
  let add_terminator ctx build_terminator =
    let (builder, _) = ctx in
    (* llvm.moe: block_terminator returns the terminator of the BB,
      insertion_block returns the current block we're inserting into with builder *)
    match L.block_terminator (L.insertion_block builder) with
      Some _ -> ()
    | None -> ignore (build_terminator ctx)
  in

  let new_scope ctx =
    let (builder, parent_scope) = ctx in
    (None, SMap.empty)
  in

  (* Codegen for function body *)
  let gen_func func =
    (* let A.Func(bind, body) = func in *)
    (* let A.Bind(typ, name) = bind in *)
    let body =  func.sbody in
    let typ = func.styp in
    let name = func.sfname in
    let formals = func.sformals in
    (* Defines the func *)
    let fn = SMap.find name all_funcs in

    (* Codegen for a statement *)
    (* Takes ctx and statement and returns a ctx *)
    let rec codegen_stmt ctx stmt =
      let (builder, sp) = ctx in
      match stmt with
      (* For expressions we just codegen the expression *)
        SExpr(e) ->
          let (ctx', ex) = codegen_expr ctx e in
          ctx'
      | SEmptyReturn -> build_default_return typ ctx
      | SReturn(expr) ->
        let (ctx', gexpr) = codegen_expr ctx expr in
        ignore (L.build_ret gexpr builder); ctx'
      (* If-statements *)
      | SIf(cond, true_stmts, false_stmts) ->
        (* Codegen the condition evaluation *)
        let (ctx', gcond) = codegen_expr ctx cond in

        (* Add the block we go to if we take this branch (cond is true) *)
        let true_blk = L.append_block context "take" fn in
        (* Add the block we go to if we don't take this branch (cond is false) *)
        let false_blk = L.append_block context "dont_take" fn in

        (* Generate the block that we come back to after both branches *)
        let join_blk = L.append_block context "join" fn in
        let join_builder = L.builder_at_end context join_blk in

        (* This function takes a builder and builds a `br` instruction, which
          (br)anches back to the start of the join_blk block made above *)
        let build_join ctx =
          let (bldr, _) = ctx in
          L.build_br join_blk bldr
        in

        (* True branch building *)
        let true_ctx = (L.builder_at_end context true_blk, new_scope ctx') in
        (* Build this branch's statements into this block *)
        let true_ctx' = codegen_stmt true_ctx true_stmts in
        add_terminator true_ctx' build_join;

        (* False branch building *)
        let false_ctx = (L.builder_at_end context false_blk, new_scope ctx') in
        let false_ctx' = codegen_stmt false_ctx false_stmts in
        add_terminator false_ctx' build_join;

        (* Build the actual conditional branch *)
        ignore (L.build_cond_br gcond true_blk false_blk builder);
        (* Finally return the new builder at end of merge *)
        let (_, sp) = ctx' in
        (join_builder, sp)
      (* While-statements *)
      | SWhile(cond, stmts) ->
        (* Start at the start block and its builder *)
        let start_blk = L.append_block context "start" fn in
        let start_builder = L.builder_at_end context start_blk in

        (* Loop block that we keep repeating *)
        let loop_blk = L.append_block context "loop" fn in
        let loop_builder = L.builder_at_end context loop_blk in
        (* Loop body (an iteration) *)
        ignore (codegen_stmt (loop_builder, new_scope ctx) stmts);
        (* Back to start after a loop iteration *)
        ignore (L.build_br start_blk loop_builder);

        (* Generate the end block where we end up after the while cond becomes false *)
        let end_blk = L.append_block context "end" fn in
        let end_builder = L.builder_at_end context end_blk in

        (* Codegen the condition evaluation *)

        let (_, gcond) = codegen_expr (start_builder, sp) cond in
        (* Build the branch instr *)
        ignore (L.build_cond_br gcond loop_blk end_blk start_builder);

        (* Branch to start *)
        ignore (L.build_br start_blk builder);

        (* Continue building after the end of the loop *)
        let (_, sp) = ctx in
        (end_builder, sp)
      (* For a block of statements, just fold *)
      | SBlock(stmts) -> List.fold_left codegen_stmt ctx stmts
    in

    let fn_builder = L.builder_at_end context (L.entry_block fn) in
    let vars = SMap.empty in
    let add_param map (ptyp, name) param =
      L.set_value_name name param;
      let local_copy = L.build_alloca (typ_to_t ptyp) name fn_builder in
      ignore (L.set_value_name (name ^ "_local") local_copy);
      ignore (L.build_store param local_copy fn_builder);
      SMap.add name local_copy map
    in
    let vars = List.fold_left2 add_param vars formals (Array.to_list (L.params fn)) in
    let fn_scope = (None, vars) in
    let fn_ctx = (fn_builder, fn_scope) in
    (* Build all statements *)
    let ctx' = codegen_stmt fn_ctx (SBlock body) in
    ignore (add_terminator ctx' (build_default_return typ))
  in

  let funcs = sfunction_decls in
    ignore (List.map gen_func funcs);
  m
