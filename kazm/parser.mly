%token EOF

%token<string> STRING_LITERAL

%token FROM IMPORT

%token PAREN_L PAREN_R BRACE_L BRACE_R
%token DOT SEMICOLON


%start program
%type <Ast.program> program
%%
program:
    blocks EOF { Program("Test") }
  | EOF { Program("empty") }

blocks:
    blocks block {}
  | block {}

block:
    import_stmt {}

import_stmt:
    FROM module_name IMPORT name { print_string "from "; print_string $2; print_string " import "; print_string $4; }

module_name:
    module_name DOT name { print_string "module_name DOT name "; print_endline $1; $1 }
  | name { print_string "name "; print_endline $1; $1 }

name:
    STRING_LITERAL { print_string "STRING_LITERAL "; print_endline $1; $1 }
