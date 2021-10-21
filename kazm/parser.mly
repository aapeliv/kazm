%token EOF

%token<string> STRING_LITERAL

%token FROM IMPORT

%token PAREN_L PAREN_R BRACE_L BRACE_R
%token DOT SEMICOLON


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

import_stmt:
    FROM module_name IMPORT name { "Importing " ^ $4 ^ " from " ^ $2 }

module_name:
    module_name DOT name { $1 ^ " . " ^ $3 }
  | name { $1 }

name:
    STRING_LITERAL { $1 }
