%token EOF

%token<string> STRING_LITERAL

%token FROM IMPORT

%token PAREN_L PAREN_R BRACE_L BRACE_R
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
    dtype_with_name PAREN_L arg_list PAREN_R BRACE_L name BRACE_R { "Declared function " ^ $1 }

closure:
    BRACE_L line BRACE_R {}

line:
    EMPTY_LINE { print_endline "empty line" }

arg_list:
    dtype_with_name { $1 }// COMMA arg_list {}
  // | {}

dtype_with_name:
    dtype name { print_endline $2; $2 }

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
  // | BOOL BRACE_L BRACE_R { "bool list" }
  // | CHAR BRACE_L BRACE_R { "char list" }
  // | INT BRACE_L BRACE_R { "int list" }
  // | DOUBLE BRACE_L BRACE_R { "double list" }
