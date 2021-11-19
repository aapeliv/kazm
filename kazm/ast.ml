(* External function name, string input *)
type a_call = Call of string * string

(* Name, list of calls *)
type a_func = Func of string * a_call list

(* Functions *)
type program = PFuncs of a_func list
