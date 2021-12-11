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
  let build_default_return typ builder =
    match typ with
      A.Void -> L.build_ret_void builder; builder
    | A.Bool -> L.build_ret (L.const_int i1_t 0) builder; builder
    | A.Int -> L.build_ret (L.const_int i32_t 0) builder; builder
    | A.Double -> L.build_ret (L.const_float double_t 0.) builder; builder
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
    add_func_def all_funcs func.sfname (typ_to_t func.styp) []
  in

  (* let A.PFuncs(funcs) = prog in *)
  (* let all_funcs = List.fold_left codegen_func_sig all_funcs funcs in *)
  let all_funcs = List.fold_left codegen_func_sig all_funcs sfunction_decls in

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
    (* Assign expression e to a new bind(type, name) *)
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
    | _ -> raise (Failure ("sast cannot be matched"))
  in

  (* Add terminator to end of a basic block *)
  let add_terminator builder build_terminator =
    (* llvm.moe: block_terminator returns the terminator of the BB,
      insertion_block returns the current block we're inserting into with builder *)
    match L.block_terminator (L.insertion_block builder) with
      Some _ -> ()
    | None -> ignore (build_terminator builder)
  in

  (* Codegen for function body *)
  let gen_func func =
    (* let A.Func(bind, body) = func in *)
    (* let A.Bind(typ, name) = bind in *) 
    let body =  func.sbody in
    let typ = func.styp in 
    let name = func.sfname in 
    (* Defines the func *)
    let fn = SMap.find name all_funcs in

    (* Codegen for a statement *)
    (* Takes builder and statement and returns a builder *)
    let rec codegen_stmt builder = function
      (* For expressions we just codegen the expression *)
        SExpr(e) -> codegen_expr builder e; builder
      (* For a block of statements, just fold *)
      | SBlock(es) -> List.fold_left codegen_stmt builder es
      | SEmptyReturn -> build_default_return typ builder
      | SReturn(expr) -> ignore (L.build_ret (codegen_expr builder expr) builder); builder
      (* If-statements *)
      | SIf(cond, true_stmts, false_stmts) ->
        (* Codegen the condition evaluation *)
        let gcond = codegen_expr builder cond in

        (* Add the block we go to if we take this branch (cond is true) *)
        let true_blk = L.append_block context "take" fn in
        (* Add the block we go to if we don't take this branch (cond is false) *)
        let false_blk = L.append_block context "dont_take" fn in

        (* Generate the block that we come back to after both branches *)
        let join_blk = L.append_block context "join" fn in
        let join_builder = L.builder_at_end context join_blk in

        (* This function takes a builder and builds a `br` instruction, which
          (br)anches back to the start of the join_blk block made above *)
        let build_join = L.build_br join_blk in

        (* True branch building *)
        let true_builder = L.builder_at_end context true_blk in
        (* Build this branch's statements into this block *)
        let true_builder_done = codegen_stmt true_builder true_stmts in
        add_terminator true_builder_done build_join;

        (* False branch building *)
        let false_builder = L.builder_at_end context false_blk in
        let false_builder_done = codegen_stmt false_builder false_stmts in
        add_terminator false_builder_done build_join;

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
    let builder_done = codegen_stmt fn_builder (SBlock body) in
    ignore (add_terminator builder_done (build_default_return typ))
  in

  let funcs = sfunction_decls in
    ignore (List.map gen_func funcs);
  m
