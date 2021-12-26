type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
And | Or | Mod

type uop = Neg | Not

type class_t = string
type typ = Int | Bool | Double | Void | String | Char | Float | ClassT of class_t | ArrayT of typ * int
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
| Call of string * expr list
| Noexpr
| ArrayAssign of expr * expr * expr
| ArrayLit of expr list
| ArrayIndex of expr * expr
| ArrayDecl of typ * expr * string 
| ArrayExp of typ * string * expr list
| ArrayLength of expr 

type stmt =
Block of stmt list
| Expr of expr
| Return of expr
| If of expr * stmt * stmt
| For of expr * expr * expr * stmt
| While of expr * stmt
| Break
| EmptyReturn

type func_decl = {
typ : typ;
fname : string;
formals : bind list;
locals : bind list;
body : stmt list;
}

type class_decl = {
cname : class_t;
cvars : bind list;
}

type program = bind list * func_decl list * class_decl list

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

let rec string_of_typ = function
Int -> "int"
| Bool -> "bool"
| Double -> "double"
| Void -> "void"
| String -> "string"
| Char -> "char"
| ArrayT(arrtyp, len) ->" arr " ^ string_of_typ arrtyp ^"["^ string_of_int len ^ "]"
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
(* | Assign(v, e) -> v ^ " = " ^ string_of_expr e *)
| Call(f, el) ->
f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
| ArrayLit(l) -> "[" ^ (String.concat ", " (List.map string_of_expr l)) ^ "]"
| ArrayAssign(id, idx, v) -> string_of_expr id ^ "[" ^ string_of_expr idx ^ "] = " ^ string_of_expr v
| ArrayIndex(id, idx) -> string_of_expr id ^ "[" ^ string_of_expr idx ^ "]" 
| ArrayDecl(t, idx, id) -> string_of_typ (t) ^ "[" ^ string_of_expr idx ^ "] " ^ id
| ArrayExp(ty, str, exp) -> "ArrayExp"
| ArrayLength(obj) ->  "length of " ^ string_of_expr obj 
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
String.concat "" (List.map string_of_vdecl fdecl.locals) ^
String.concat "" (List.map string_of_stmt fdecl.body) ^
"}\n"

let string_of_program (vars, funcs) =
String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
String.concat "\n" (List.map string_of_fdecl funcs)


