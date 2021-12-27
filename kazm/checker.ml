open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.
   Check each global variable, check each function, and check each class *)

let check (globals, functions, classes) =

  (* Verify a list of bindings has no void types or duplicate names *)
  let check_binds (kind : string) (binds : bind list) =
    List.iter (function
    (Void, b) -> raise (Failure ("illegal void " ^ kind ^ " " ^ b))
      | _ -> ()) binds;
    let rec dups = function
        [] -> ()
      |    ((_,n1) :: (_,n2) :: _) when n1 = n2 ->
      raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (**** Check global variables ****)
  check_binds "global" globals;

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls =
    let add_bind map (name, ty) = StringMap.add name {
      typ = Void;
      fname = name;
      formals = [(ty, "x")];
      body = [] } map
    in let smap =  StringMap.add "next_int" {
      typ = Int;
      fname = "next_int";
      formals = [];
      body = [] } StringMap.empty 
    in List.fold_left add_bind smap [ ("print", String);
                               ("println", String);
                               ("int_print", Int);
                               ("int_println", Int);
                               ("double_print", Double);
                               ("double_println", Double)]
  in

  (* Add function name to symbol table *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err
       | _ ->  StringMap.add n fd map
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in

  (* Add class name to class symbol table with cvars*)
  let add_class map cls = 
    let dup_error = "duplicate class " ^ cls.cname
    and make_err er = raise (Failure er) 
    and n = cls.cname
    and vars = List.fold_left (fun m (ty, name) -> StringMap.add name ty m) StringMap.empty cls.cvars
    in match cls with
        _ when StringMap.mem n map -> make_err dup_error
      | _ -> StringMap.add n vars map
  in

  (* Collect all class info into class symbol table *)
  let class_decls = List.fold_left add_class StringMap.empty classes 
  in

  (* Collect all class_method info into class_method symbol table *)
  let class_methods_decls = List.fold_left 
          (fun map cls -> StringMap.add cls.cname 
              (List.fold_left add_func StringMap.empty cls.cmethods) map) StringMap.empty classes
  in

  (* Return a function from our class_method symbol table *)
  (*let find_classmethod clsname methodname =
  *if StringMap.mem clsname class_decls = false then raise (Failure ("unrecognized class " ^ clsname))
  *  else try StringMap.find methodname (StringMap.find clsname class_decls)
  *       with Not_found -> raise (Failure ("unrecognized function " ^ methodname ^ " in class " ^ clsname))
  *in *)

  (* Return a function from our function symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  (**** Check functions ****)
  let check_function vars func =
    (* Make sure no formals are void or duplicates *)
    check_binds "formal" func.formals;

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
                    StringMap.empty (globals @ func.formals @ vars)
    in

    (* Return a variable type from our local symbol table *)
    let type_of_identifier s locals =
      try StringMap.find s locals
      with Not_found -> try StringMap.find s symbols
               with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return a ref (e.g. car.power) type *)
    let trace_type cls_instance cls_var locals =
      match type_of_identifier cls_instance locals with
        ClassT(clsname) -> (if StringMap.mem clsname class_decls = false 
                            then raise (Failure ("no class type " ^ clsname))
                            else let vars_map = StringMap.find clsname class_decls in
                                  try StringMap.find cls_var vars_map
                                  with Not_found -> raise (Failure ("undeclared variable " ^ cls_var)))
        | _ -> raise (Failure (cls_instance ^ " must be a class instance"))
    in

    (* Return a method function *)
    let trace_method clsname methodname =
        if StringMap.mem clsname class_decls = false 
        then raise (Failure ("no class type " ^ clsname))
        else let methods_map = StringMap.find clsname class_methods_decls in
              try StringMap.find methodname methods_map
              with Not_found -> raise (Failure ("undeclared method " ^ methodname ^ " in class " ^ clsname))
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr e locals =
      match e with
        Literal  l -> (Int, SLiteral l)
      | Dliteral l -> (Double, SDliteral l)
      | BoolLit l  -> (Bool, SBoolLit l)
      | CharLit c -> (Char, SCharLit c)
      | StringLit s -> (String, SStringLit s)
      | Noexpr     -> (Void, SNoexpr)
      | Id (s :: []) -> (type_of_identifier s locals, SId([s])) (* TODO: check if hd class is valid *)
      | Id (s :: var :: []) ->  (trace_type s var locals, SId(s :: [var]))
      | Id (_) -> raise(Failure ("Usage: cls_instance.cls_var (e.g. car.power)"))
      | Assign(ref, e) as ex ->
          let lt = match ref with
            var :: [] -> type_of_identifier var locals
          | s :: var :: [] -> trace_type s var locals
          | _ -> raise (Failure ("Usage: cls_instance.cls_var (e.g. car.power)"))
          and (rt, e') = expr e locals in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                      string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign(ref, (rt, e')))
      | Unop(op, e) as ex ->
          let (t, e') = expr e locals in
          let ty = match op with
            Neg when t = Int || t = Double -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))
      | Binop(e1, op, e2) as e ->
          let (t1, e1') = expr e1 locals
          and (t2, e2') = expr e2 locals in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          let ty = match op with
            Add | Sub | Mult when same && t1 = Int -> Int 
          | Div | Mod when same && t1 = Int   -> if e2' = SLiteral(0) 
            then raise(Failure("Div by 0: " ^ string_of_expr e)) else Int
          | Add | Sub | Mult when same && t1 = Double -> Double
          | Div when same && t1 = Double -> if e2' = SDliteral("0.")
            then raise(Failure("Div by 0.0: " ^ string_of_expr e)) else Double
          | Equal | Neq            when same               -> Bool
          | Less | Leq | Greater | Geq
                     when same && (t1 = Int || t1 = Double) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
          Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Call(ref, args) as call ->
          let fd = match ref with
            fname :: [] -> find_func fname
          | s :: methodname :: [] -> (match type_of_identifier s locals with 
                                      ClassT(clsname) -> trace_method clsname methodname
                                    | _ -> raise (Failure (s ^ " must be a class instance")))
          | _ -> raise (Failure ("Usage: cls_instance.cls_var (e.g. car.power)"))
          in 
            let param_length = List.length fd.formals in
            if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ string_of_expr call))
            else let check_call (ft, _) e =
              let (et, e') = expr e locals in
              let err = "illegal argument found " ^ string_of_typ et ^
                " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
              in (check_assign ft et err, e')
            in
            let args' = List.map2 check_call fd.formals args
            in (fd.typ, SCall(ref, args'))
    in

    let check_bool_expr e locals =
      let (t', e') = expr e locals
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e')
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt stmt locals =
      match stmt with
        Expr e -> SExpr (expr e locals)
      | Initialize (_, _) -> raise (Failure ("Initialize stmts are inside Block."))
      | If(p, b1, b2) -> SIf(check_bool_expr p locals, check_stmt b1 locals, check_stmt b2 locals)
      | For(e1, e2, e3, st) ->
      SFor(expr e1 locals, check_bool_expr e2 locals, expr e3 locals, check_stmt st locals)
      | While(p, s) -> SWhile(check_bool_expr p locals, check_stmt s locals)
      | EmptyReturn -> SEmptyReturn
      | Break -> SBreak
      | Return e -> let (t, e') = expr e locals in
        if t = func.typ then SReturn (t, e')
        else raise (
      Failure ("return gives " ^ string_of_typ t ^ " expected " ^
            string_of_typ func.typ ^ " in " ^ string_of_expr e))

        (* A block is correct if each statement is correct and nothing
            follows any Return statement.  Nested blocks are flattened. *)
      | Block sl ->
          let rec check_stmt_list stmts locals = 
            match stmts with
              [Return _ as s] -> [check_stmt s locals]
            | Initialize (bd, None) :: ss -> 
                let (typ, name) = bd in
                if StringMap.mem name locals = true 
                          then raise (Failure ("cannot initialize " ^ name ^ " twice"))
                          else SInitialize(bd, None) :: check_stmt_list ss (StringMap.add name typ locals)
            | Initialize (bd, Some e) :: ss -> 
                let (typ, name) = bd in
                let se = expr e locals in
                if StringMap.mem name locals = true 
                          then raise (Failure ("cannot initialize " ^ name ^ " twice"))
                          else SInitialize(bd, Some se) :: check_stmt_list ss (StringMap.add name typ locals)
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) locals (* Flatten blocks *)
            | s :: ss         -> check_stmt s locals :: check_stmt_list ss locals
            | []              -> []
          in SBlock(check_stmt_list sl locals)

    in (* body of check_function *)
    { styp = func.typ;
      sfname = func.fname;
      sformals = func.formals;
      sbody = match check_stmt (Block func.body) StringMap.empty with
    SBlock(sl) -> sl
      | _ -> raise (Failure ("internal error: block didn't become a block?"))
    }
  in

  let check_class cls =
    { scname = cls.cname;
      scvars = cls.cvars;
      scmethods = List.map (check_function cls.cvars) cls.cmethods;}
  in
  (globals, List.map (check_function []) functions, List.map check_class classes)