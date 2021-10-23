%token EOF

%token<string> STRING_LITERAL

%token FROM IMPORT

%token PAREN_L PAREN_R BRACE_L BRACE_R SQB_L SQB_R
%token DOT SEMICOLON COMMA
%token EMPTY_LINE
%token VOID BOOL CHAR INT DOUBLE

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
    dtype_with_name PAREN_L arg_list PAREN_R closure {
      "Declared function " ^ $1 ^ " with arg list " ^ (List.fold_left (fun a b -> a ^ ", " ^ b) "" (List.rev $3)) ^ " and body: " ^ $5
    }

closure:
    BRACE_L line BRACE_R { $2 }

line:
    EMPTY_LINE { "empty line" }

arg_list:
    arg_list COMMA dtype_with_name { $3::$1 }
  | dtype_with_name { $1::[] }

dtype_with_name:
    dtype name { $2 ^ " (t: " ^ $1 ^ ")" }

name:
    STRING_LITERAL { $1 }

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
