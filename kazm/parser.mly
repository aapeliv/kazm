/* Ocamlyacc parser for Kazm */

%{ open Ast %}

%token FROM IMPORT

%token PAREN_L PAREN_R BRACE_L BRACE_R SQB_L SQB_R /* ( ) { } [ ] */
%token DOT SEMI COMMA MOD ASSIGN  /* . ; , * % = */
%token PLUS MINUS TIMES DIVIDE  /* + - * / */
%token PLUSEQ MINUSEQ TIMESEQ DIVIDEQ /* + - * / += -= *= /= */
%token AND OR NOT  /* && || ! */
%token EQ NEQ LT LEQ GT GEQ /* == != < <= > >= */
%token EMPTY
%token VOID BOOL CHAR INT DOUBLE 
%token IF THEN ELSE ELSIF FOR WHILE DO 
%token RETURN BREAK CONTINUE

%token<string> STRING_LITERAL   /* could change STRING_LITERAL to just STRING */
%token<int> INT_LITERAL 
%token EOF

%nonassoc NOELSE
%nonassoc ELSE 
%left SEMICO 
%left IF THEN 
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
    blocks EOF { Program($1) }
  | EOF { Program("{empty program}") }

blocks:
    blocks block { $1 ^ ";\n\n" ^ $2}
  | block { $1 }

block:
    import_stmt { $1 }
  | func { $1 }

import_stmt:
    FROM module_name IMPORT name { "Importing " ^ $4 ^ " from " ^ $2 }

module_name:
    module_name DOT name { $1 ^ " . " ^ $3 }
  | name { $1 }

func:
    dtype_with_name PAREN_L arg_list PAREN_R func_body {
      "Declared function " ^ $1 ^ " with arg list " ^ (List.fold_left (fun a b -> a ^ ", " ^ b) "" (List.rev $3)) ^ " and body: " ^ $5
    }

func_body:
    BRACE_L stmts BRACE_R { "stmts: " ^ (List.fold_left (fun a b -> a ^ "\n" ^ b) "" (List.rev $2)) }

stmts:
    stmts SEMI stmt { $3::$1 }
  | stmts SEMI { $1 }
  | stmt { $1::[] }

stmt:
    EMPTY { "empty" }
  | return_stmt { $1 }
  | conditional_stmt { $1 }
  | assign_stmt { $1 }
  | call_stmt { $1 }

return_stmt:
    RETURN stmt { "Return: " ^ $2 }

conditional_stmt:
    IF PAREN_L stmt PAREN_R BRACE_L stmts BRACE_R { "lone if with cond: " ^ $3 }

decl_var_stmt:
    dtype_with_name ASSIGN stmt { "assigning new var " ^ $3 ^ " to " ^ $1 }

assign_stmt:
    name ASSIGN stmt { "assigning " ^ $3 ^ " to " ^ $1 }

call_stmt:
    name PAREN_L name_list PAREN_R { "calling " ^ $1 ^ " with name_list " ^ (List.fold_left (fun a b -> a ^ ", " ^ b) "" (List.rev $3)) }

arg_list:
    arg_list COMMA dtype_with_name { $3::$1 }
  | dtype_with_name { $1::[] }
 
assignment_operator:
    ASSIGN { Assign }
  | PLUSEQ { Pluseq }
  | MINUSEQ { Minuseq }
  | TIMESEQ { TimeseQ }
  | DIVIDEQ { Divideq }

expr: 
    INT_LITERAL        { Intlit($1)               }
  | STRING_LITERAL     { Stringlit($1)            }
  | expr PLUS expr     { Binop($1, Add, $3)       }
  | expr MINUS expr    { Binop($1, Sub, $3)       }
  | expr TIMES expr    { Binop($1, Mul, $3)       }
  | expr DIVIDE expr   { Binop($1, Div, $3)       }
  | expr MOD expr      { Binop($1, Mod, $3)       }
  | expr PLUSEQ expr   { Binop($1, Pluseq, $3)    }
  | expr MINUSEQ expr  { Binop($1, Minuseq, $3)   }
  | expr TIMESEQ expr  { Binop($1, Timeseq, $3)   }
  | expr DIVIDEQ expr  { Binop($1, Divideq, $3)   }
  | expr EQ expr       { Binop($1, Equal, $3)     }
  | expr NEQ expr      { Binop($1, Neq, $3)       }
  | expr LT expr       { Binop($1, Less, $3)      }
  | expr LEQ expr      { Binop($1, Leq, $3)       }
  | expr GT expr       { Binop($1, Greater, $3)   }
  | expr GEQ expr      { Binop($1, Geq, $3)       }
  | expr AND expr      { Binop($1, And, $3)       }
  | expr OR expr       { Binop($1, Or, $3)        }
  | NOT expr           { Unop(Not, $2)            }



dtype_with_name:
    dtype name { $2 ^ " (t: " ^ $1 ^ ")" }

name_list:
    name_list COMMA name { $3::$1 }
  | name { $1::[] }

name:
    STRING_LITERAL { $1 }

dtype:
    VOID { Void }
  /* primitives */ 
  | BOOL { Bool }
  | CHAR { Char }
  | INT { Int }
  | DOUBLE { Double }
  /* arrays */ 
  | dtype SQB_L SQB_R { Array($1) }