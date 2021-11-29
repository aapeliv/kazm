open Ast

let check_call call = function
    Call("println", _) -> ()
  | Call("print", _) -> ()
  | Call(name, _) -> raise (Failure ("Unknown function " ^ name))

let check_func func =
  let Func(name, calls) = func in
    if String.equal name "main" then List.map check_call calls else raise (Failure ("You can only define main, got " ^ name))

let check prog =
  (* Since the syntax is so simple, just check the called function is valid *)
  let PFuncs(funcs) = prog in
    List.map check_func funcs;
    if List.exists (function Func(name, _) -> (String.equal name "main")) funcs then () else raise (Failure ("Missing main"))
