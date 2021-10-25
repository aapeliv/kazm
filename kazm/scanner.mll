{ open Parser }

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| "from" { FROM }
| "import" { IMPORT }
| '(' { PAREN_L }
| ')' { PAREN_R }
| '{' { BRACE_L }
| '}' { BRACE_R }
| '[' { SQB_L }
| ']' { SQB_R }
| '.' { DOT }
| ';' { SEMI }
| ',' { COMMA }
| '%' { MOD }
| '=' { ASSIGN  }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| "+=" { PLUSEQ }
| "-=" { MINUSEQ }
| "*+" { TIMESEQ }
| "/=" { DIVIDEEQ }
| "&&" { AND }
| "||" { OR }
| '!' { NOT } (* "!" in C-Net *)
| "==" { EQ }
| "!=" { NEQ }
| '<' { LT }
| "<=" { LEQ }
| '>' { GT }
| ">=" { GEQ }
| "empty" { EMPTY }
| "void" { VOID }
| "bool" { BOOL }
| "char" { CHAR }
| "int" { INT }
| "double" { DOUBLE }
| "if" { IF }
| "then" { THEN }
| "else" { ELSE }
| "elseif" { ELSEIF }
| "for" { FOR }
| "while" { WHILE }
| "do" { DO }
| "return" { RETURN }
| "break" { BREAK }
| "continue" { CONTINUE }
| ['0'-'9']+ as intlit { INT_LITERAL(int_of_string intlit) }
| ['a'-'z''_']+ as strlit { STRING_LITERAL(strlit) }
| eof { EOF }
