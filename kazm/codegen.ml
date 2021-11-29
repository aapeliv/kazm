module A = Ast
module L = Llvm

let gen prog =
  (* Set up module & context *)
  let context = L.global_context () in
  let m = L.create_module context "kazm" in

  (* Set up types in the context *)
  let i1_t = L.i1_type context in
  let i8_t = L.i8_type context in
  let i32_t = L.i32_type context in
  let char_t = i8_t in
  let void_t = L.void_type context in
  let char_ptr_t = L.pointer_type char_t in

  (* Types and decls of builtins, currently print and println *)
  let print_t : L.lltype =
    L.function_type void_t [| char_ptr_t |] in
  let print_func : L.llvalue =
    L.declare_function "print" print_t m in
  let println_func : L.llvalue =
    L.declare_function "println" print_t m in

  let rec codegen_expr builder = function
      A.Call(cname, carg) ->
        let arg_str = L.build_global_stringptr carg "arg" builder in
        (match cname with
          "println" -> L.build_call println_func [| arg_str |] "" builder
        | "print" -> L.build_call print_func [| arg_str |] "" builder
        | _ -> raise (Failure ("Calling unkonwn function " ^ cname ^ ". Can only call println...")))
    | A.BoolLit(truthy) -> L.const_int i1_t (if truthy then 1 else 0)
    | A.IntLit(value) -> L.const_int i32_t value
  in

  let typ_to_t = function
      A.Void -> void_t
    | A.Bool -> i1_t
    | A.Int -> i32_t
  in

  let rec codegen_stmt builder = function
      A.Expr(e) -> ignore (codegen_expr builder e)
    | A.Block(e_lst) -> ignore (List.map (codegen_stmt builder) e_lst)
    | A.Assign(bind, e) -> ignore (
      let A.Bind(typ, name) = bind in
      let lh = L.build_alloca (typ_to_t typ) name builder in
      L.build_store (codegen_expr builder e) lh builder
    )
    (* | A.If(cond, s) ->  *)
  in

  let gen_func func =
    let A.Func(name, calls) = func in
    (* Defines the func *)
    (* TODO: make everything not void, TODO: allow arguments *)
    let lfunc = L.define_function name (L.function_type void_t [| |]) m in
    let builder = L.builder_at_end context (L.entry_block lfunc) in
    (* Create all the calls *)
    ignore (List.map (codegen_stmt builder) calls);
    L.build_ret_void builder
  in

  let A.PFuncs(funcs) = prog in
    ignore (List.map gen_func funcs);
  m
