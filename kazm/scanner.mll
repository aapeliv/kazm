{ open Parser }

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| '.' { DOT }
| ';' { SEMICOLON }
| '(' { PAREN_L }
| ')' { PAREN_R }
| '{' { BRACE_L }
| '}' { BRACE_R }
| "from" { FROM }
| "import" { IMPORT }
| ['a'-'z']+ as slit { STRING_LITERAL(slit) }
| eof { EOF }
