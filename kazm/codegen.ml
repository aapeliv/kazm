module A = Ast
module L = Llvm
open Sast

module SMap = Map.Make(String)

(* A variable scope, contains variables and a ref to the parent scope *)
type vscope = Scope of (vscope option) * (L.llvalue * A.typ * bool) SMap.t
(* A codegen context: builder and variable scope *)
type ctx_t = Ctx of L.llbuilder * vscope

let gen (sfunction_decls, sclass_decls) =
  (* Set up module & context *)
  let context = L.global_context () in
  let m = L.create_module context "kazm" in

  let mangle_method_name cname mname = cname ^ "__" ^ mname in

  (* Set up types in the context *)
  let i1_t = L.i1_type context in
  let i8_t = L.i8_type context in
  let i32_t = L.i32_type context in
  let i64_t = L.i64_type context in
  let double_t = L.double_type context in
  let string_t = L.pointer_type (L.i8_type context) in
  let char_t = i8_t in
  let void_t = L.void_type context in
  let char_ptr_t = L.pointer_type char_t in
  let void_ptr_t = L.pointer_type i8_t in

  let gen_cls_t map cls =
    let name = cls.scname in
    let cls_t = L.named_struct_type context name in
    SMap.add name cls_t map
  in

  let cls_ts = List.fold_left gen_cls_t SMap.empty sclass_decls in

  (* Map our AST type to LLVM type *)
  let rec typ_to_t = function
      A.Void -> void_t
    | A.Bool -> i1_t
    | A.Int -> i32_t
    | A.String -> string_t
    | A.Double -> double_t
    | A.ClassT(name) ->
      let cls_t = SMap.find name cls_ts in
      L.pointer_type cls_t
    | A.Char -> i8_t
    | A.ArrT(ty, _) -> L.pointer_type (typ_to_t ty)
  in

  let get_default = function
    | A.Bool -> L.const_int i1_t 0
    | A.Int -> L.const_int i32_t 0
    | A.Double -> L.const_float double_t (float_of_string "0.0")
    | A.Char -> L.const_int i8_t 109
  in

  (* Codegen function definitions *)
  let get_func_sig func =
    let ret_t = typ_to_t func.styp in
    let arg_types = List.map (fun sformal -> typ_to_t (fst sformal)) func.sformals in
    (ret_t, arg_types)
  in

  let codegen_class_decl map cls =
    let name = cls.scname in
    let member_ts = List.map (fun v -> typ_to_t (fst v)) cls.scvars in
    let cls_t = SMap.find name cls_ts in
    let add_method map mthd =
      let mname = mthd.sfname in
      let mangled_name = mangle_method_name name mname in
      let (ret_t, arg_ts) = get_func_sig mthd in
      let me_t = L.pointer_type cls_t in
      let func_t = L.function_type ret_t (Array.of_list (me_t::arg_ts)) in
      let func_def = L.define_function mangled_name func_t m in
      SMap.add mname (mthd, mangled_name, func_def) map
    in
    let dtrs = match cls.scdestructor with
      None -> []
    | Some d -> [d]
    in
    let mthds = List.fold_left add_method SMap.empty cls.scmethods in
    let ctrs = List.fold_left add_method SMap.empty cls.scconstructors in
    let dtr = List.fold_left add_method SMap.empty dtrs in
    ignore (L.struct_set_body cls_t (Array.of_list member_ts) false);
    SMap.add name (cls, cls_t, mthds, ctrs, dtr) map
  in

  let all_classes = List.fold_left codegen_class_decl SMap.empty sclass_decls in

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

  (* Given a builder and our type, build a dummy return (e.g. if there's a missing return) *)
  let build_default_return typ ctx =
    let Ctx(builder, sp) = ctx in
    match typ with
      A.Void -> ignore (L.build_ret_void builder); ctx
    | A.Bool -> ignore (L.build_ret (L.const_int i1_t 0) builder); ctx
    | A.Int -> ignore (L.build_ret (L.const_int i32_t 0) builder); ctx
    | A.Double -> ignore (L.build_ret (L.const_float double_t 0.) builder); ctx
    | A.Char -> ignore (L.build_ret (L.const_int i8_t 109) builder); ctx
  in

  let all_funcs = SMap.empty in
  (* Builtins... *)
  let all_funcs = add_func_decl all_funcs "print" void_t [char_ptr_t] in
  let all_funcs = add_func_decl all_funcs "println" void_t [char_ptr_t] in
  let all_funcs = add_func_decl all_funcs "int_print" void_t [i32_t] in
  let all_funcs = add_func_decl all_funcs "int_println" void_t [i32_t] in
  let all_funcs = add_func_decl all_funcs "double_print" void_t [double_t] in
  let all_funcs = add_func_decl all_funcs "double_println" void_t [double_t] in
  let all_funcs = add_func_decl all_funcs "char_println" void_t [i8_t] in
  let all_funcs = add_func_decl all_funcs "next_int" i32_t [] in

  (* Codegen function definitions *)
  let codegen_func_sig all_funcs func =
    let (ret_t, arg_ts) = get_func_sig func in
    add_func_def all_funcs func.sfname ret_t arg_ts
  in

  let all_funcs = List.fold_left codegen_func_sig all_funcs sfunction_decls in

  let new_scope ctx =
    let Ctx(_, parent_scope) = ctx in
    Scope(Some parent_scope, SMap.empty)
  in

  let rec debug_scope_inspect name scope =
    (* Prints out all vars in all scopes *)
    match scope with
      Scope(None, map) -> _debug_print_all_vars_in_map "top" map
    | Scope(Some parent, map) -> _debug_print_all_vars_in_map name map; debug_scope_inspect (name ^ "_up") parent
  and _debug_print_all_vars_in_map name map =
    SMap.iter (fun name _ -> ignore (print_endline ("Found var " ^ name ^ " in " ^ name)))
  in

  let rec find_var scope name =
    match scope with
      Scope(None, map) -> SMap.find name map
    | Scope(Some parent, map) -> if SMap.mem name map then SMap.find name map else find_var parent name
  in

  let add_var scope name var vtyp own =
    let Scope(p, map) = scope in
    let map' = SMap.add name (var, vtyp, own) map in
    Scope(p, map')
  in

  let build_class_alloc cname name builder =
    (* Class info *)
    let (cls, cls_t, mthds, ctrs, dtr) = SMap.find cname all_classes in
    (* A pointer to the right struct *)
    let ptr_var = L.build_alloca (L.pointer_type cls_t) name builder in
    (* Malloc the memory *)
    let mallocd = L.build_malloc cls_t ("_malloc_" ^ name) builder in
    ignore (L.build_store mallocd ptr_var builder);
    let me = L.build_load ptr_var "me" builder in
    (* Let's call the constructor, if any *)
    (match SMap.find_opt cname ctrs with
      None -> ()
    | Some (mthd, mangled_name, fn) -> ignore (L.build_call fn (Array.of_list ([me])) "" builder));
    ptr_var
  in

  let destroy_var name var vtyp builder =
    match vtyp with
    (* TODO: *)
      (* For classes, call the destructor and free the memory *)
      | A.ClassT(cname) ->
        (* Class info *)
        let (cls, cls_t, mthds, ctrs, dtr) = SMap.find cname all_classes in
        let me = L.build_load var "me" builder in
        (* Call destructor if it exists *)
        (match SMap.find_opt cname dtr with
          None -> ()
        | Some (mthd, mangled_name, fn) -> ignore (L.build_call fn (Array.of_list ([me])) "" builder));
        ignore (L.build_free me builder)
      (* For arrays, call the desctructors on each and free the memory *)
      | A.ArrT(ty, _) ->
        ()
      (* For primitive types, we don't have to do anything *)
      | _ -> ()
  in

  let find_fq_var builder scope = function
    (* Unqualified access *)
      ref::[] -> let (var, _, _) = find_var scope ref in var
    (* Qualified access *)
    | hd::tl::[] ->
      (* Get info about the variable *)
      let (cval, ClassT(cname), own) = find_var scope hd in
      (* Get info about the class *)
      let (cls, cls_t, mthds, ctrs, dtr) = SMap.find cname all_classes in
      (* Members names with indexes *)
      let mems = List.mapi (fun ix v -> (ix, snd v)) cls.scvars in
      (* Filter out the members that have the same name as the sought after member (there should only be 1) *)
      let (mem_pos_in_class, _) = List.hd (List.filter (fun (ix, v) -> (tl = v)) mems) in
      (* Load address of the struct *)
      let load = L.build_load cval ("_struct_" ^ hd) builder in
      L.build_struct_gep load mem_pos_in_class (tl ^ "_ptr") builder
    | _ -> raise (Failure("find_fq_var: cannot be other patterns"))
  in

  let build_scope_exit ctx =
    (* Builds destructors, etc *)
    let Ctx(builder, sp) = ctx in
    let Scope(_, vars) = sp in
    let dest_var name (var, vtyp, own) =
      if own then destroy_var name var vtyp builder else ()
    in
    ignore (SMap.iter dest_var vars)
  in

  (* Codegen for an expression *)
  let rec codegen_expr ctx ((typ, e) : sexpr) =
    let Ctx(builder, sp) = ctx in
    match e with
    (* Function call *)
      SCall(ref, exprs) ->
        let (ctx', args) = Future.fold_left_map codegen_expr ctx exprs in
        let ex = match ref with
        | fname :: [] ->
          L.build_call (SMap.find fname all_funcs) (Array.of_list args) "" builder
        | cvar :: methodname :: [] ->
          let (var, A.ClassT(cname), own) = find_var sp cvar in
          let (cls, cls_t, mthds, ctrs, dtr) = SMap.find cname all_classes in
          let (mthd, mangled_name, fn) = SMap.find methodname mthds in
          let me = L.build_load var "me" builder in
          L.build_call fn (Array.of_list (me::args)) "" builder
      | _ -> raise (Failure("codegen_expr SCall:cannot be other patterns"))
        in
    (ctx', ex)
    (* New bool literal *)
    | SBoolLit(value) -> (ctx, L.const_int i1_t (if value then 1 else 0))
    (* New 32-bit integer literal *)
    | SLiteral(value) -> (ctx, L.const_int i32_t value)
    | SDliteral(value) -> (ctx, L.const_float double_t (float_of_string value))
    (* New string literal (just make a new global string) *)
    | SStringLit(value) -> (ctx, L.build_global_stringptr value "globalstring" builder)
    | SCharLit(value) -> (ctx, L.const_int i8_t (Char.code (String.get value 0)))
    | SUnop(op, ((t, _) as e)) ->
        let (ctx1, e') = codegen_expr ctx e in
        let lbuild = match op with
            A.Neg when t = A.Double -> L.build_fneg
          | A.Neg                  -> L.build_neg
          | A.Not                  -> L.build_not
        in
        let Ctx(builder, sp) = ctx1 in
        let new_expr = lbuild e' "unop_res" builder in
        (ctx1, new_expr)
    | SBinop(e1, op, e2) ->
      (* Lookup right thing to build in llvm *)
      let lbuild = match op with
          A.Add when typ = Int -> L.build_add
        | A.Add when typ = Double -> L.build_fadd
        | A.Sub when typ = Int -> L.build_sub
        | A.Sub when typ = Double -> L.build_fsub
        | A.Mult -> L.build_mul
        (* TODO: Simply tested but more tests may be needed *)
        | A.Div when typ = Int -> L.build_sdiv
        (* | A.Div when typ = Double ->L.build_fdiv *)
        | A.Div when typ = Double ->  let (Double, SDliteral l) = e2 in
                                      raise(Failure(l))
        | A.Mod -> L.build_srem
        | A.And -> L.build_and
        | A.Or -> L.build_or
        | A.Equal -> L.build_icmp L.Icmp.Eq
        | A.Neq -> L.build_icmp L.Icmp.Ne
        | A.Less -> L.build_icmp L.Icmp.Slt
        | A.Leq -> L.build_icmp L.Icmp.Sle
        | A.Greater -> L.build_icmp L.Icmp.Sgt
        | A.Geq -> L.build_icmp L.Icmp.Sge
      in
      let (ctx1, first) = codegen_expr ctx e1 in
      let (ctx2, second) = (codegen_expr ctx1 e2) in
      let Ctx(builder, sp) = ctx2 in
      let new_expr = lbuild first second "binop_res" builder in
      (ctx2, new_expr)
    | SId(fqn) ->
      let var = find_fq_var builder sp fqn in
      (ctx, L.build_load var "id" builder)
    (* Assign expression e to a new bind(type, name) *)
    | SAssign(fqn, value) ->
      let var = find_fq_var builder sp fqn in
      let (ctx', e') = codegen_expr ctx value in
      ignore (L.build_store e' var builder);
      (ctx, e')
    | SArrayLit(arr_t, exs) ->
      (* arr: sexpr list = typ * sx list *)
      let size = List.length exs in
      let ty = typ_to_t (A.ArrT(arr_t, size)) in
      (* allocate memory for array *)
      let arr_alloca = L.build_array_malloc ty (L.const_int i32_t size) "array_literal" builder in
      (* bitcast -- pointer-to-int *)
      let arr_ptr = L.build_pointercast arr_alloca ty "array_ptr" builder in
      (* store an element in slot `ix` and pass off context to the next one *)
      let store_el (ctx, ix) el =
        let (ctx', gex) = codegen_expr ctx (arr_t, el) in
        let element_ptr = L.build_in_bounds_gep arr_ptr [|  (L.const_int i32_t ix) |] "array_element" builder in
        ignore (L.build_store gex element_ptr builder);
        (ctx', ix + 1)
      in
      let (ctx', _) = List.fold_left store_el (ctx, 0) exs in
      (ctx', arr_ptr)
    | SArrayAccess(name, pos_ex) ->
      let (ctx', element) = build_array_element_ptr ctx name pos_ex in
      (ctx', L.build_load element (name ^ "__element") builder)
    | SArrayAssign(name, pos_ex, assign_ex) ->
      let (ctx', assign) = codegen_expr ctx assign_ex in
      let (ctx'', element) = build_array_element_ptr ctx' name pos_ex in
      (ctx'', L.build_store assign element builder)
    | SArrayLength(name) -> 
      let var = find_var sp name in 
      let (ll, vtyp, own) = var in (* llvalue and Ast typ *)
      let ArrT(t, l) = vtyp in (* Ast typ and length of array *)
      (ctx, L.const_int i32_t l) 
  and build_array_element_ptr ctx name pos_ex =
    let Ctx(builder, sp) = ctx in
    let (ctx', pos) = codegen_expr ctx pos_ex in
    let array_ptr = find_fq_var builder sp [name] in
    (* Load the array address into a register *)
    let array = L.build_load array_ptr (name ^ "__array") builder in
    (ctx', L.build_in_bounds_gep array [| pos |] (name ^ "__element_ptr") builder)
  in

  (* Add terminator to end of a basic block *)
  let add_terminator ctx build_terminator =
    let Ctx(builder, _) = ctx in
    (* llvm.moe: block_terminator returns the terminator of the BB,
      insertion_block returns the current block we're inserting into with builder *)
    match L.block_terminator (L.insertion_block builder) with
      Some _ -> ()
    | None -> ignore (build_terminator ctx)
  in

  let build_alloc vtyp name builder =
    match vtyp with
      | A.ClassT(cname) ->
        build_class_alloc cname name builder
      | _ ->
        L.build_alloca (typ_to_t vtyp) name builder
  in

  (* Codegen for function body *)
  let raw_gen_func body typ name formals fn =
    (* Codegen for a statement *)
    (* Takes ctx and statement and returns a ctx *)
    let rec codegen_stmt ctx stmt =
      let Ctx(builder, sp) = ctx in
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
          let Ctx(bldr, _) = ctx in
          L.build_br join_blk bldr
        in

        (* True branch building *)
        let true_ctx = Ctx(L.builder_at_end context true_blk, new_scope ctx') in
        (* Build this branch's statements into this block *)
        let true_ctx' = codegen_stmt true_ctx true_stmts in
        ignore (build_scope_exit true_ctx');
        add_terminator true_ctx' build_join;

        (* False branch building *)
        let false_ctx = Ctx(L.builder_at_end context false_blk, new_scope ctx') in
        let false_ctx' = codegen_stmt false_ctx false_stmts in
        ignore (build_scope_exit false_ctx');
        add_terminator false_ctx' build_join;

        (* Build the actual conditional branch *)
        ignore (L.build_cond_br gcond true_blk false_blk builder);
        (* Finally return the new builder at end of merge *)
        let Ctx(_, sp) = ctx' in
        Ctx(join_builder, sp)
      (* While-statements *)
      | SWhile(cond, stmt) ->
        (* Start at the start block and its builder *)
        let start_blk = L.append_block context "start" fn in
        let start_builder = L.builder_at_end context start_blk in

        (* Loop block that we keep repeating *)
        let loop_blk = L.append_block context "loop" fn in
        let loop_builder = L.builder_at_end context loop_blk in
        (* Loop body (an iteration) *)
        let while_ctx = Ctx(loop_builder, new_scope ctx) in
        let while_ctx' = codegen_stmt while_ctx stmt in
        ignore (build_scope_exit while_ctx');
        (* Back to start after a loop iteration *)
        let Ctx(loop_builder', _) = while_ctx' in
        ignore (L.build_br start_blk loop_builder');

        (* Generate the end block where we end up after the while cond becomes false *)
        let end_blk = L.append_block context "end" fn in
        let end_builder = L.builder_at_end context end_blk in

        (* Codegen the condition evaluation *)

        let start_ctx = Ctx(start_builder, sp) in
        let (_, gcond) = codegen_expr start_ctx cond in
        (* Build the branch instr *)
        ignore (L.build_cond_br gcond loop_blk end_blk start_builder);

        (* Branch to start *)
        ignore (L.build_br start_blk builder);

        (* Continue building after the end of the loop *)
        let Ctx(_, sp) = ctx in
        Ctx(end_builder, sp)
      (* For a block of statements, just fold *)
      | SBlock(stmts) ->
        let ctx' = Ctx(builder, new_scope ctx) in
        let ctx'' = List.fold_left codegen_stmt ctx' stmts in
        ignore (build_scope_exit ctx'');
        let Ctx(_, sp') = ctx' in
        let Ctx(builder'', _) = ctx'' in
        Ctx(builder'', sp')
      | SInitialize((vtyp, name), expr) ->
          (match vtyp with
            A.ClassT(cname) ->
              if expr != None then raise (Failure ("Can't assign init class")) else
              Ctx(builder, add_var sp name (build_class_alloc cname name builder) vtyp true)
            | _ ->
              let (ctx', value) =
                (match expr with
                | Some e ->
                  codegen_expr ctx e
                | None ->
                  match vtyp with
                    | A.ArrT(t, l) ->
                      let arr_lit = (match t with
                            A.Int -> SArrayLit(A.Int, List.init l (fun x -> SLiteral(0)))
                          | A.Bool -> SArrayLit(A.Bool, List.init l (fun x -> SBoolLit(false)))
                          | A.Double -> SArrayLit(A.Double, List.init l (fun x -> SDliteral("0.0")))
                        ) in
                      codegen_expr ctx (A.ArrT(t, l), arr_lit)
                    | A.String ->
                      (ctx, L.build_global_stringptr "" "default_string" builder)
                    | _ -> (ctx, get_default vtyp)
                )
              in
              let var = L.build_alloca (typ_to_t vtyp) name builder in
              let Ctx(_, sp') = ctx' in
              ignore (L.build_store value var builder);
              Ctx(builder, add_var sp' name var vtyp true)
        )
    in

    let fn_builder = L.builder_at_end context (L.entry_block fn) in

    let add_param map (ptyp, name) param =
      L.set_value_name name param;
      let local_copy = L.build_alloca (typ_to_t ptyp) name fn_builder in
      ignore (L.set_value_name (name ^ "_local") local_copy);
      ignore (L.build_store param local_copy fn_builder);
      SMap.add name (local_copy, ptyp, false) map
    in
    let params = List.fold_left2 add_param SMap.empty formals (Array.to_list (L.params fn)) in
    let fn_scope = Scope(None, params) in
    let initialize_var scope (vtyp, name, own) =
      add_var scope name (build_alloc vtyp name fn_builder) vtyp own
    in
    let fn_scope' = List.fold_left initialize_var fn_scope [] in
    let fn_ctx = Ctx(fn_builder, fn_scope') in
    (* Build all statements *)
    let ctx' = codegen_stmt fn_ctx (SBlock body) in
    ignore (build_scope_exit ctx');
    ignore (add_terminator ctx' (build_default_return typ))
  in

  let gen_func func =
    let body = func.sbody in
    let typ = func.styp in
    let name = func.sfname in
    let formals = func.sformals in
    let fn = SMap.find name all_funcs in
    raw_gen_func body typ name formals fn
  in

  let gen_methods cname (cls, cls_t, mthds, ctrs, dtr) =
    let gen_method name (mthd, mangled_name, fn) =
      let body = mthd.sbody in
      let typ = mthd.styp in
      let me_formal = (A.ClassT(cname), "me") in
      let formals = me_formal :: mthd.sformals in
      raw_gen_func body typ mangled_name formals fn
    in
    ignore (SMap.iter gen_method mthds);
    ignore (SMap.iter gen_method ctrs);
    ignore (SMap.iter gen_method dtr);
  in

  ignore (List.map gen_func sfunction_decls);
  ignore (SMap.iter gen_methods all_classes);

  m
