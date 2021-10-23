{ open Parser }

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| '.' { DOT }
| ',' { COMMA }
| ';' { SEMICOLON }
| '(' { PAREN_L }
| ')' { PAREN_R }
| '{' { BRACE_L }
| '}' { BRACE_R }
| "from" { FROM }
| "import" { IMPORT }
| ['a'-'z']+ as slit { STRING_LITERAL(slit) }
| "emptyline" { EMPTY_LINE }
| "void" { VOID }
| "bool" { BOOL }
| "char" { CHAR }
| "int" { INT }
| "double" { DOUBLE }
| eof { EOF }
