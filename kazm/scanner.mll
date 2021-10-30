{ open Parser }

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
(* single line comment starts with // and carries to end of line *)
| "//" [^'\n']* { tokenize lexbuf }
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
| "/=" { DIVIDEQ }
| "&&" { AND }
| "||" { OR }
| '!' { NOT } (* "!" in C-Net *)
| "==" { EQ }
| "!=" { NEQ }
| '<' { LT }
| "<=" { LEQ }
| '>' { GT }
| ">=" { GEQ }
| "class" { CLASS }
| "void" { VOID }
| "bool" { BOOL }
| "char" { CHAR }
| "int" { INT }
| "double" { DOUBLE }
| "if" { IF }
| "else" { ELSE }
| "for" { FOR }
| "while" { WHILE }
| "return" { RETURN }
| "break" { BREAK }
| "true" { TRUE }
| "false" { FALSE }
| "\"" ([^'\"']+ as str) "\"" { STRING_LITERAL(str) }
| ['0'-'9']+ as int { INT_LITERAL(int_of_string int) }
| ['A'-'Z''a'-'z''_''0'-'9']+ as str { IDENTIFIER(str) }
| eof { EOF }
