/* Ocamlyacc parser for Kazm */

%{
open Ast

let join_str_list str_list delimiter = List.fold_left (fun a b -> a ^ delimiter ^ b) "" (List.rev str_list)
let concat_stmts stmts = join_str_list stmts "\n"
let concat_list list = join_str_list list ", "
%}

%token IMPORT

%token PAREN_L PAREN_R BRACE_L BRACE_R SQB_L SQB_R /* ( ) { } [ ] */
%token DOT SEMI COMMA MOD ASSIGN  /* . ; , * % = */
%token PLUS MINUS TIMES DIVIDE  /* + - * / */
%token PLUSEQ MINUSEQ TIMESEQ DIVIDEQ /* + - * / += -= *= /= */
%token AND OR NOT  /* && || ! */
%token EQ NEQ LT LEQ GT GEQ /* == != < <= > >= */
%token EMPTY
%token VOID BOOL CHAR INT DOUBLE
%token IF ELSE FOR WHILE
%token RETURN BREAK
%token CLASS ME
%token TRUE FALSE
%token CONTINUE

%token<string> NAME
%token<string> STRING_LITERAL
%token<int> INT_LITERAL
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%left SEMICO
%left IF
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT GEQ LEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT

%start program
%type <Ast.program> program
%%

program:
    blocks EOF { Program(concat_stmts $1) }
  | EOF { Program("{empty program}") }

blocks:
    blocks block { $2::$1 }
  | block { $1::[] }

block:
  func { $1 }
  | import_stmt { $1 }

import_stmt:
    IMPORT name { "Importing " ^ $2 }

func:
    dtype_with_name PAREN_L arg_list PAREN_R BRACE_L stmts BRACE_R {
      "Declared function " ^ $1 ^ " with arg list " ^ (concat_list $3) ^ " and body: " ^ concat_stmts $6
    }

stmts:
    stmts stmt { $2::$1 }
  | stmt { $1::[] }

stmt:
    EMPTY { "empty" }
  | expr SEMI { $1 }
  | return_stmt SEMI { $1 }
  | assign_stmt SEMI { $1 }
  | decl_var_stmt SEMI { $1 }
  | if_stmt { $1 }
  | while_stmt { $1 }

return_stmt:
    RETURN expr { "Return: " ^ $2 }

if_stmt:
    IF PAREN_L expr PAREN_R BRACE_L stmts BRACE_R ELSE BRACE_L stmts BRACE_R { "if with catch-all else" }
  | IF PAREN_L expr PAREN_R BRACE_L stmts BRACE_R ELSE if_stmt { "continuation if" }
  | IF PAREN_L expr PAREN_R BRACE_L stmts BRACE_R { "start of if" }

while_stmt:
    WHILE PAREN_L expr PAREN_R BRACE_L stmts BRACE_R { "while (" ^ $3 ^ ") {\n" ^ (concat_list $6) ^ "\n}" }

arg_list:
    arg_list COMMA dtype_with_name { $3::$1 }
  | dtype_with_name { $1::[] }

expr:
    INT_LITERAL        { string_of_int $1 }
  | STRING_LITERAL     { "string_literal: " ^ $1 }
  | expr PLUS expr     { $1 ^ " + " ^ $3 }
  | expr MINUS expr    { $1 ^ " - " ^ $3 }
  | expr TIMES expr    { $1 ^ " * " ^ $3 }
  | expr DIVIDE expr   { $1 ^ " / " ^ $3 }
  | expr MOD expr      { $1 ^ " % " ^ $3 }
  | expr EQ expr       { $1 ^ " == " ^ $3 }
  | expr NEQ expr      { $1 ^ " != " ^ $3 }
  | expr LT expr       { $1 ^ " < " ^ $3 }
  | expr LEQ expr      { $1 ^ " <= " ^ $3 }
  | expr GT expr       { $1 ^ " > " ^ $3 }
  | expr GEQ expr      { $1 ^ " >= " ^ $3 }
  | expr AND expr      { $1 ^ " && " ^ $3 }
  | expr OR expr       { $1 ^ " || " ^ $3 }
  | NOT expr           { " ! " ^ $2 }
  | PAREN_L expr PAREN_R { "(" ^ $2 ^ ")" }
  // refer to a name
  | name               { $1 }
  // call a function
  | call_expr          { $1 }

expr_list:
    expr_list COMMA expr { $3::$1 }
  | expr { $1::[] }

call_expr:
    name PAREN_L expr_list PAREN_R { "calling " ^ $1 ^ " with expr_list " ^ (concat_list $3) }

assign_stmt:
    name ASSIGN expr   { $1 ^ " = " ^ $3 }
  | name PLUSEQ expr   { $1 ^ " += " ^ $3 }
  | name MINUSEQ expr  { $1 ^ " -= " ^ $3 }
  | name TIMESEQ expr  { $1 ^ " *= " ^ $3 }
  | name DIVIDEQ expr  { $1 ^ " /= " ^ $3 }

decl_var_stmt:
    dtype_with_name ASSIGN expr { "assigning new var " ^ $3 ^ " to " ^ $1 }

dtype_with_name:
    dtype name { $2 ^ " (t: " ^ $1 ^ ")" }

name:
    NAME { $1 }

dtype:
    VOID { "void" }
  // primitives
  | BOOL { "bool" }
  | CHAR { "char" }
  | INT { "int" }
  | DOUBLE { "double" }
  // arrays
  | BOOL SQB_L SQB_R { "bool list" }
  | CHAR SQB_L SQB_R { "char list" }
  | INT SQB_L SQB_R { "int list" }
  | DOUBLE SQB_L SQB_R { "double list" }
