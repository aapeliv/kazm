type expr =
    (* External function name, string input *)
    Call of string * string
  | BoolLit of bool

type stmt =
    Expr of expr

(* Name, list of calls *)
type a_func = Func of string * stmt list

(* Functions *)
type program = PFuncs of a_func list
