{ open Parser }

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| '=' { EQUALS }
| '.' { DOT }
| ',' { COMMA }
| ';' { SEMI }
| '(' { PAREN_L }
| ')' { PAREN_R }
| '{' { BRACE_L }
| '}' { BRACE_R }
| '[' { SQB_L }
| ']' { SQB_R }
| "from" { FROM }
| "import" { IMPORT }
| "empty" { EMPTY }
| "void" { VOID }
| "bool" { BOOL }
| "char" { CHAR }
| "int" { INT }
| "double" { DOUBLE }
| "if" { IF }
| "else" { ELSE }
| "elseif" { ELSEIF }
| "then" { THEN }
| "return" { RETURN }
| ['a'-'z''_']+ as slit { STRING_LITERAL(slit) }
| eof { EOF }
