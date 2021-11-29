type typ = Void | Int | Bool

type bind = Bind of typ * string

type expr =
    (* External function name, string input *)
    Call of string * string
  | IntLit of int
  | BoolLit of bool

type stmt =
    Expr of expr
  | Block of stmt list
  | If of expr * stmt
  | Assign of bind * expr

(* Name, list of calls *)
type a_func = Func of string * stmt list

(* Functions *)
type program = PFuncs of a_func list
