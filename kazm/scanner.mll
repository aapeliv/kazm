{ open Parser }


let digit = ['0'-'9']
let exp = 'e'['-''+']?['0'-'9']+
let double = (
    ((digit)+'.'(digit)* (exp)?) |
    ((digit)* '.'(digit)+(exp)?) |
    ((digit)+exp)
)

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
<<<<<<< HEAD
=======
| "//" [^'\n']* { tokenize lexbuf }
>>>>>>> 19fd09861b46d51198e2e88d34c53e9fab5dcd28
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
<<<<<<< HEAD
| "continue" { CONTINUE }
| "true" {TRUE}
| "false" {FALSE}
| ['0'-'9']+ as intlit { INT_LITERAL(int_of_string intlit) }
| ['a'-'z''_']+ as strlit { ID(strlit) }
| double as doublelit {DOUBLE_LITERAL(doublelit)}
| '''     { STRING_LITERAL (parsestringSQ (Buffer.create 100) lexbuf) }
| '"'     { STRING_LITERAL (parsestringDQ (Buffer.create 100) lexbuf) }
=======
| "\"" ([^'\"']+ as str) "\"" { STRING_LITERAL(str) }
| ['0'-'9']+ as int { INT_LITERAL(int_of_string int) }
| ['a'-'z''_''0'-'9']+ as str { NAME(str) }
>>>>>>> 19fd09861b46d51198e2e88d34c53e9fab5dcd28
| eof { EOF }
| _ as character     { raise (Failure("Undefined character: " ^ Char.escaped character)) }

and parsestringSQ buffer = parse
    '''                 { Buffer.contents buffer }
  | newline             { Buffer.add_string buffer (Lexing.lexeme lexbuf); parsestringSQ buffer lexbuf }
  | [^ '''  '\n' '\r']+ { Buffer.add_string buffer (Lexing.lexeme lexbuf); parsestringSQ buffer lexbuf }
  | eof                 { raise (Failure("Non-terminated single quotes")) }

and parsestringDQ buffer = parse
    '"'                 { Buffer.contents buffer }
  | newline             { Buffer.add_string buffer (Lexing.lexeme lexbuf); parsestringDQ buffer lexbuf }
  | [^ '"'  '\n' '\r']+ { Buffer.add_string buffer (Lexing.lexeme lexbuf); parsestringDQ buffer lexbuf }
  | eof                 { raise (Failure("Non-terminated double quotes")) }