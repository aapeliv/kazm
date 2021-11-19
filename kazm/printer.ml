open Ast

let string_of_call = function
    Call(name, input) -> name ^ "('" ^ input ^ "');"

let string_of_func = function
    Func(name, call_list) -> "void " ^ name ^ "() {" ^ List.fold_left (fun a b -> a ^ "\n" ^ b) "" (List.map string_of_call (List.rev call_list)) ^ "\n}"

let rec string_of_program prog =
  let PFuncs(funcs) = prog in
    List.fold_left (fun a b -> a ^ "\n" ^ b) "" (List.map string_of_func (List.rev funcs))
