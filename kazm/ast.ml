type typ = Void | Int | Bool

type bind = Bind of typ * string

type expr =
    Call of string * expr list
  | IntLit of int
  | BoolLit of bool
  | StrLit of string

type stmt =
    Expr of expr
  | Block of stmt list
  | If of expr * stmt
  | Assign of bind * expr

(* Name, list of calls *)
type a_func = Func of string * stmt list

(* Functions *)
type program = PFuncs of a_func list
