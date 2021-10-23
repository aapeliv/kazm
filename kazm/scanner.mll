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
| '[' { SQB_L }
| ']' { SQB_R }
| "from" { FROM }
| "import" { IMPORT }
| "emptyline" { EMPTY_LINE }
| "void" { VOID }
| "bool" { BOOL }
| "char" { CHAR }
| "int" { INT }
| "double" { DOUBLE }
| ['a'-'z''_']+ as slit { STRING_LITERAL(slit) }
| eof { EOF }
