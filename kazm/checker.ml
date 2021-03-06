open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.
   Check each function and each class *)

let check (functions, classes) =

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
                               ("double_println", Double);
                               ("char_println", Char)]
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

  (* Add class name to class symbol table with cvars *)
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
  let class_decls = List.fold_left add_class StringMap.empty classes in

  (* Collect all class_method info into class_method symbol table *)
  let class_methods_decls = List.fold_left
          (fun map cls -> StringMap.add cls.cname
              (List.fold_left add_func StringMap.empty cls.cmethods) map) StringMap.empty classes
  in

  (* Return a function from our function symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let check_main func_typ =
    match func_typ with
      Int -> true
    | _ ->  raise (Failure("main function type is expected to be int instead of " ^ string_of_typ func_typ)) 
  in

  let main_func = find_func "main" in 
  let main_func_typ = main_func.typ in
  let _ = check_main main_func_typ in

  (* Check functions *)
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
                    StringMap.empty (func.formals @ vars)
    in

    (* Return a variable type from our local symbol table *)
    let type_of_identifier s scope =
      try StringMap.find s scope
      with Not_found -> try StringMap.find s symbols
               with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return a ref (e.g. car.power) type *)
    let trace_type cls_instance cls_var scope =
      match type_of_identifier cls_instance scope with
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
    let rec expr scope e =
      match e with
        Literal  l -> (Int, SLiteral l)
      | Dliteral l -> (Double, SDliteral l)
      | BoolLit l  -> (Bool, SBoolLit l)
      | CharLit c -> (Char, SCharLit c)
      | StringLit s -> (String, SStringLit s)
      | Noexpr     -> (Void, SNoexpr)
      | Id (s :: []) -> (type_of_identifier s scope, SId([s])) (* TODO: check if hd class is valid *)
      | Id (s :: var :: []) ->  (trace_type s var scope, SId(s :: [var]))
      | Id (_) -> raise(Failure ("Usage: cls_instance.cls_var (e.g. car.power)"))
      | Assign(ref, e) as ex ->
          let lt = match ref with
            var :: [] -> type_of_identifier var scope
          | s :: var :: [] -> trace_type s var scope
          | _ -> raise (Failure ("Usage: cls_instance.cls_var (e.g. car.power)"))
          and (rt, e') = expr scope e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                      string_of_typ rt ^ " in " (* ^ string_of_expr ex *)
          in (check_assign lt rt err, SAssign(ref, (rt, e')))
      | Unop(op, e) as ex ->
          let (t, e') = expr scope e in
          let ty = match op with
            Neg when t = Int || t = Double -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))
      | Binop(e1, op, e2) as e ->
          let (t1, e1') = expr scope e1
          and (t2, e2') = expr scope e2 in
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
          | s :: methodname :: [] -> (match type_of_identifier s scope with
                                      ClassT(clsname) -> trace_method clsname methodname
                                    | _ -> raise (Failure (s ^ " must be a class instance")))
          | _ -> raise (Failure ("Usage: cls_instance.cls_var (e.g. car.power)"))
          in
            let param_length = List.length fd.formals in
            if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ string_of_expr call))
            else let check_call (ft, _) e =
              let (et, e') = expr scope e in
              let err = "illegal argument found " ^ string_of_typ et ^
                " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
              in (check_assign ft et err, e')
            in
            let args' = List.map2 check_call fd.formals args
            in (fd.typ, SCall(ref, args'))
      | ArrayLit(values) ->
        let array_body = List.map (expr scope) values in
        (* Type of first element in array *)
        let array_t, _ = List.hd array_body in
        let type_check el =
          let el_t = fst el in
          if el_t != array_t then raise (Failure ("Types in array literal must all match")) else ()
        in
        (* Check all the types match *)
        ignore (List.map type_check array_body);
        (ArrT(array_t, List.length values), SArrayLit(array_t, List.map snd array_body))
      | ArrayAccess(v, e) -> (* array name and array index *)
        (* check if type of e is an int *)
        let (typ', sx') = expr scope e in
          if typ' != Int
          then raise(Failure("Wrong type of array index in array access"))
          else
            let v_ty = type_of_identifier v scope in
            let e_ty = match v_ty with
              ArrT(t, l) ->  match e with
                               Literal l1 -> if l1 >= l then raise(Failure("Array (" ^ v ^") index (" ^
                                              string_of_expr e ^ ") out of bounds (" ^ string_of_int l ^")")) else t 
                              |_ -> t
            | _ -> raise(Failure("Wrong type of variable in array access"))
            in (e_ty, SArrayAccess(v, (typ', sx')))
      | ArrayAssign(v, e1, e2) -> (* array name array index value to be assigned *)
        (* check if type of e1 is int *)
        let (typ', sx') = expr scope e1 in
          if typ' != Int
          then raise(Failure("Wrong type of array index in array access"))
          else (* check if type of v is array *)
            let v_ty = type_of_identifier v scope in
            let e_ty = match v_ty with
                ArrT(t, l) -> match e1 with
                               Literal l1 -> if l1 >= l then raise(Failure("Array (" ^ v ^") index (" ^
                                              string_of_expr e ^ ") out of bounds (" ^ string_of_int l ^")")) else t 
                              |_ -> t
              | _ -> raise(Failure("Wrong type of variable in array assign"))
            in
            let (typ'', sx'') = expr scope e2 in
            (e_ty, SArrayAssign(v, (typ', sx'), (typ'', sx'')))
      | ArrayLength(name) -> (*return the length of array *)
        let v_ty = type_of_identifier name scope in
        let e_ty = match v_ty with 
            ArrT(t, l) -> Int
          | _ -> raise(Failure("Must call .length on an array. " ^ name ^ " is not an array"))
        in 
        (e_ty, SArrayLength(name))
    in

    let check_bool_expr e scope =
      let (t', e') = expr scope e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e')
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt stmt scope =
      match stmt with
        Expr e -> SExpr (expr scope e)
      | Initialize (_, _) -> raise (Failure ("Initialize stmts are inside Block."))
      | If(p, b1, b2) -> SIf(check_bool_expr p scope, check_stmt b1 scope, check_stmt b2 scope)
      | For(e1, e2, e3, st) -> check_stmt (Block([Expr(e1); While(e2, Block([st; Expr(e3)]))])) scope
      | While(p, s) -> SWhile(check_bool_expr p scope, check_stmt s scope)
      | EmptyReturn -> SEmptyReturn
      | Return e -> let (t, e') = expr scope e in
        if t = func.typ then SReturn (t, e')
        else raise (
      Failure ("return gives " ^ string_of_typ t ^ " expected " ^
            string_of_typ func.typ ^ " in " ^ string_of_expr e))

      | StmtScope(block) ->
        check_stmt block scope
        (* A block is correct if each statement is correct and nothing
            follows any Return statement.  Nested blocks are flattened. *)
      | Block sl ->
          let rec check_stmt_list stmts scope =
            match stmts with
              [Return _ as s] -> [check_stmt s scope]
            | Initialize ((ClassT c, name), None) :: ss ->
                if StringMap.mem c class_decls = false then raise (Failure (c ^" class " ^ "is undefined"))
                else SInitialize((ClassT c, name), None) :: check_stmt_list ss (StringMap.add name (ClassT c) scope)
            | Initialize (bd, None) :: ss ->
                let (typ, name) = bd in
                if StringMap.mem name scope = true
                then raise (Failure ("cannot initialize " ^ name ^ " twice"))
                else SInitialize(bd, None) :: check_stmt_list ss (StringMap.add name typ scope)
            | Initialize ((ClassT c, name), Some e) :: ss ->
                let se = expr scope e in
                if StringMap.mem c class_decls = false then raise (Failure (c ^" class " ^ "is undefined"))
                else SInitialize((ClassT c, name), None) :: check_stmt_list ss (StringMap.add name (ClassT c) scope)
            | Initialize (bd, Some e) :: ss ->
                let (typ, name) = bd in
                let se = expr scope e in
                let (typ', sx') = se in 
                let _ = match typ with 
                    ArrT(t, l) -> let e' = (match e with 
                        ArrayLit(ex) -> ex 
                      | _ -> raise(Failure(name ^" needs to be initialized to an array literal")))
                    in 
                    (* Check if array is declared and init with wrong length & wrong type *)
                    if (List.length e') != l then raise(Failure("Array (" ^ name ^ ") " ^
                      "declared with length (" ^ string_of_int l ^") but init with length (" ^ string_of_int (List.length e') ^")" )) else t 
                  | _ -> typ
                in
                if typ <> typ' then raise(Failure("initialize: variable and value to be assigned of different types"))
                else 
                  (if StringMap.mem name scope = true
                            then raise (Failure ("cannot initialize " ^ name ^ " twice"))
                            else SInitialize(bd, Some se) :: check_stmt_list ss (StringMap.add name typ scope))
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) scope
            | s :: ss         -> check_stmt s scope :: check_stmt_list ss scope
            | []              -> []
          in SBlock(check_stmt_list sl scope)

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
    let ctrs = cls.cconstructors in
    let dtrs = cls.cdestructors in
    let check_cd_name func =
      if func.fname <> cls.cname then raise (Failure ("Class constructor or destructor does not have same name as class, expected " ^ cls.cname ^ " (class) == " ^ func.fname)) else ()
    in
    ignore (List.map check_cd_name ctrs);
    ignore (List.map check_cd_name dtrs);
    let check_dtrs = function
        [] -> None
      | hd::[] -> Some (check_function ((ClassT(cls.cname), "me")::[]) hd)
      | _ -> raise (Failure ("Can only have one or zero destructors"))
    in
    { scname = cls.cname;
      scvars = cls.cvars;
      scmethods = List.map (check_function ((ClassT(cls.cname), "me")::[])) cls.cmethods;
      scconstructors = List.map (check_function ((ClassT(cls.cname), "me")::[])) ctrs;
      scdestructor = check_dtrs dtrs;
    }
  in
  (List.map (check_function []) functions, List.map check_class classes)
