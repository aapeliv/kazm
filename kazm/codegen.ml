module A = Ast
module L = Llvm

module SMap = Map.Make(String)

let gen prog =
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

  let all_funcs = SMap.empty in
  (* Builtins... *)
  let all_funcs = add_func_decl all_funcs "print" void_t [char_ptr_t] in
  let all_funcs = add_func_decl all_funcs "println" void_t [char_ptr_t] in
  let all_funcs = add_func_decl all_funcs "int_print" void_t [i32_t] in
  let all_funcs = add_func_decl all_funcs "int_println" void_t [i32_t] in
  let all_funcs = add_func_decl all_funcs "double_print" void_t [double_t] in
  let all_funcs = add_func_decl all_funcs "double_println" void_t [double_t] in

  (* Codegen function definitions *)
  let codegen_func_sig all_funcs func =
    let A.Func(bind, _) = func in
    let A.Bind(typ, name) = bind in
    add_func_def all_funcs name (typ_to_t typ) []
  in

  let A.PFuncs(funcs) = prog in
  let all_funcs = List.fold_left codegen_func_sig all_funcs funcs in

  (* Codegen for an expression *)
  let rec codegen_expr builder = function
    (* Function call *)
      A.Call(cname, exprs) ->
        (* let arg_str = L.build_global_stringptr carg "arg" builder in *)
        let arg_array = Array.of_list (List.map (codegen_expr builder) exprs) in
        (* todo: need to make sure these functions actually exist, etc *)
        L.build_call (SMap.find cname all_funcs) arg_array "" builder
    (* New bool literal *)
    | A.BoolLit(value) -> L.const_int i1_t (if value then 1 else 0)
    (* New 32-bit integer literal *)
    | A.IntLit(value) -> L.const_int i32_t value
    | A.DoubleLit(value) -> L.const_float double_t value
    (* New string literal (just make a new global string) *)
    | A.StrLit(value) -> L.build_global_stringptr value "globalstring" builder
    | A.Binop(e1, op, e2) ->
      (* Lookup right thing to build in llvm *)
      let lbuild = match op with
          OpPlus -> L.build_add
        | OpMinus -> L.build_sub
        | OpTimes -> L.build_mul
        | OpDivide -> L.build_sdiv
        | OpMod -> L.build_srem
      in
      lbuild (codegen_expr builder e1) (codegen_expr builder e2) "im" builder
  in

  (* Codegen for a statement *)
  let rec codegen_stmt builder = function
    (* For expressions we just codegen the expression *)
      A.Expr(e) -> ignore (codegen_expr builder e)
    (* Fir a block of statements, just call us recursively on each *)
    | A.Block(e_lst) -> ignore (List.map (codegen_stmt builder) e_lst)
    (* Assign expression e to a *new* bind(type, name) *)
    | A.Assign(bind, e) -> ignore (
      let A.Bind(typ, name) = bind in
      (* Left hand side of the assignment, allocate memory and create a var *)
      let lh = L.build_alloca (typ_to_t typ) name builder in
      (* Codegen the expression and store in this var *)
      L.build_store (codegen_expr builder e) lh builder
      )
    | A.ReturnVoid -> ignore (L.build_ret_void builder)
    | A.Return(expr) -> ignore (L.build_ret (codegen_expr builder expr) builder)
    (* | A.If(cond, s) ->  *)
  in

  (* Codegen for function body *)
  let gen_func func =
    let A.Func(bind, calls) = func in
    let A.Bind(typ, name) = bind in
    (* Defines the func *)
    let lfunc = SMap.find name all_funcs in
    let builder = L.builder_at_end context (L.entry_block lfunc) in
    (* Create all the calls *)
    ignore (List.map (codegen_stmt builder) calls);
  in

  let A.PFuncs(funcs) = prog in
    ignore (List.map gen_func funcs);
  m
