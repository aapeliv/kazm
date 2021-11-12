open Ast

let check_call call =
  let Call(name, arg) = call in
    if name = "println" then () else raise (Failure ("Unknown function " ^ name))

let check_func func =
  let Func(name, calls) = func in
    List.map check_call calls

let check prog =
  (* Since the syntax is so simple, just check the called function is valid *)
  let PFuncs(funcs) = prog in List.map check_func funcs
