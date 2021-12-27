type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Mod

type uop = Neg | Not

type class_t = string
type typ = Int | Bool | Double | Void | String | Char | Float | ClassT of class_t | Arr of typ * int
type bind = typ * string

type ref = string list

type expr =
    Literal of int
  | Dliteral of string
  | BoolLit of bool
  | StringLit of string
  | CharLit of string
  | Id of ref
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of ref * expr
  | Call of ref * expr list
  | Noexpr
  | ArrayLit of expr list

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Break
  | EmptyReturn
  | Initialize of bind * expr option

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    body : stmt list;
}

type class_decl = {
    cname : class_t;
    cvars : bind list;
    cmethods : func_decl list;
}

type program = bind list * func_decl list * class_decl list

(* TODO: maybe fix later *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Mod -> "%"


let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Double -> "double"
  | Void -> "void"
  | String -> "string"
  | Char -> "char"
  | ClassT(name) -> "class " ^ name
  
let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Dliteral(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | StringLit(s) -> "\"" ^ s ^ "\""
  | CharLit(c) -> "\'" ^ c ^ "\'"
  | Id(s) -> String.concat ", " s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> String.concat " " v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      String.concat " " f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | EmptyReturn -> "return;\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Break -> "break;"


let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_cdecl cdecl =
  "class" ^ " " ^ cdecl.cname ^ " " ^
  cdecl.cname ^ "\n{\n" ^ String.concat ", " (List.map string_of_vdecl cdecl.cvars) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_fdecl cdecl.cmethods) ^
  "}\n"

let string_of_program (vars, funcs, classes) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) ^ "\n" ^
  String.concat "\n" (List.map string_of_cdecl classes)

