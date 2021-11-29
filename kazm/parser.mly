/* Ocamlyacc parser for Kazm */

%{
open Ast

(* let join_str_list str_list delimiter = List.fold_left (fun a b -> a ^ delimiter ^ b) "" (List.rev str_list)
let concat_stmts stmts = join_str_list stmts "\n"
let concat_list list = join_str_list list ", " *)
%}

%token PAREN_L PAREN_R BRACE_L BRACE_R SQB_L SQB_R SQB_PAIR /* ( ) { } [ ] */
%token DOT SEMI COMMA MOD ASSIGN  /* . ; , * % = */
%token PLUS MINUS TIMES DIVIDE  /* + - * / */
%token PLUSEQ MINUSEQ TIMESEQ DIVIDEQ /* + - * / += -= *= /= */
%token AND OR NOT  /* && || ! */
%token EQ NEQ LT LEQ GT GEQ /* == != < <= > >= */
%token VOID BOOL CHAR INT DOUBLE
%token IF ELSE FOR WHILE
%token RETURN BREAK
%token CLASS
%token TRUE FALSE

%token<string> IDENTIFIER
%token<string> STRING_LITERAL
%token<float> DOUBLE_LITERAL
%token<char> CHAR_LITERAL
%token<int> INT_LITERAL
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%nonassoc PAREN_L PAREN_R BRACE_L BRACE_R SQB_L SQB_R
%left SEMICO
%left IF
%right ASSIGN PLUSEQ MINUSEQ TIMESEQ DIVIDEQ
%left OR
%left AND
%left EQ NEQ
%left LT GT GEQ LEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT

%left DOT

%start program
%type <Ast.program> program
%%

program:
    funcs { PFuncs(List.rev $1) }

funcs:
    funcs func { $2::$1 }
  | { [] }

func:
    dtype_with_simple_name PAREN_L PAREN_R BRACE_L stmts BRACE_R { let Bind(t, n) = $1 in Func(n, List.rev $5) }

stmts:
    stmts stmt { $2::$1 }
  | { [] }

stmt:
    expr SEMI { Expr($1) }
  | assign_new_var_expr SEMI { $1 }
  | if_stmt { $1 }

if_stmt:
    IF PAREN_L expr PAREN_R BRACE_L stmts BRACE_R { If($3, Block(List.rev $6)) }

expr:
    simple_name PAREN_L STRING_LITERAL PAREN_R { Call($1, $3) }
  | INT_LITERAL { IntLit($1) }
  | TRUE { BoolLit(true) }
  | FALSE { BoolLit(false) }

assign_new_var_expr:
    dtype_with_simple_name ASSIGN expr { Assign($1, $3) }

simple_name:
    IDENTIFIER { $1 }

dtype_with_simple_name:
    dtype simple_name { Bind($1, $2) }

dtype:
    VOID { Void }
  | INT { Int }
  | BOOL { Bool }
