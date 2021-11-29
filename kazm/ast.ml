type typ = Void | Int | Double | Bool

type bind = Bind of typ * string

type op = OpPlus | OpMinus | OpTimes | OpDivide | OpMod

type expr =
    Call of string * expr list
  | IntLit of int
  | DoubleLit of float
  | BoolLit of bool
  | StrLit of string
  (* refer to variable by name *)
  | Ref of string
  | Binop of expr * op * expr

type stmt =
    Expr of expr
  | Block of stmt list
  | If of expr * stmt
  | Assign of bind * expr
  | ReturnVoid
  | Return of expr

type a_func = Func of bind * stmt list

type program = PFuncs of a_func list
