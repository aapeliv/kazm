module A = Ast
module L = Llvm

let gen prog =
  (* Set up module & context *)
  let context = L.global_context () in
  let m = L.create_module context "kazm" in

  (* Set up types in the context *)
  let i8_t = L.i8_type context in
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

  let gen_func func =
    let A.Func(name, calls) = func in
    (* Defines the func *)
    (* TODO: make everything not void, TODO: allow arguments *)
    let lfunc = L.define_function name (L.function_type void_t [| |]) m in
    let builder = L.builder_at_end context (L.entry_block lfunc) in
    let construct_func = function
      A.Call(cname, carg) ->
        let arg_str = L.build_global_stringptr (carg ^ "\n") "arg" builder in
        if
          String.equal cname "println"
        then
          L.build_call println_func [| arg_str |] "" builder
        else
          raise (Failure ("Calling unkonwn function " ^ cname ^ ". Can only call println..."))
    in
    (* Create all the calls *)
    List.map construct_func calls;
    L.build_ret_void builder
  in

  let A.PFuncs(funcs) = prog in
    List.map gen_func funcs;
  m
