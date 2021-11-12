(* External function name, string input and junk *)
type a_call = Call of string * string * string

(* Name, list of calls and junk *)
type a_func = Func of string * a_call list * string

(* Functions and junk *)
type program = PPair of a_func list * string

type lit = Discard_input of string
