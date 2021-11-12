/* Ocamlyacc parser for Kazm */

%{
open Ast

let join_str_list str_list delimiter = List.fold_left (fun a b -> a ^ delimiter ^ b) "" (List.rev str_list)
let concat_stmts stmts = join_str_list stmts "\n"
let concat_list list = join_str_list list ", "
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
    blocks EOF { $1 }

blocks:
    blocks func { let PFuncs(funcs) = $1 in PFuncs($2::funcs) }
  | { PFuncs([]) }

func:
    dtype_with_simple_name PAREN_L PAREN_R BRACE_L stmts BRACE_R {
      Func($1, $5)
    }

stmts:
    stmts stmt { $2::$1 }
  | { [] }

stmt:
    expr SEMI { $1 }

expr:
    simple_name PAREN_L STRING_LITERAL PAREN_R { Call($1, $3) }

simple_name:
    IDENTIFIER { $1 }

dtype_with_simple_name:
    dtype simple_name { $2 }

dtype:
    VOID { "void" }
