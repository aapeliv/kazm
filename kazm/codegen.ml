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

  (* Codegen for an expression *)
  let rec codegen_expr builder = function
    (* Function call *)
      A.Call(cname, exprs) ->
        (* let arg_str = L.build_global_stringptr carg "arg" builder in *)
        let arg_array = Array.of_list (List.map (codegen_expr builder) exprs) in
        (match cname with
          "println" -> L.build_call println_func arg_array "" builder
        | "print" -> L.build_call print_func arg_array "" builder
        | _ -> raise (Failure ("Calling unkonwn function " ^ cname ^ ". Can only call println...")))
    (* New bool literal *)
    | A.BoolLit(value) -> L.const_int i1_t (if value then 1 else 0)
    (* New 32-bit integer literal *)
    | A.IntLit(value) -> L.const_int i32_t value
    (* New string literal (just make a new global string) *)
    | A.StrLit(value) -> L.build_global_stringptr value "globalstring" builder
  in

  (* Map our AST type to LLVM type *)
  let typ_to_t = function
      A.Void -> void_t
    | A.Bool -> i1_t
    | A.Int -> i32_t
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
    (* | A.If(cond, s) ->  *)
  in

  (* Codegen for function body *)
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
