{ open Parser }

let newline = '\n' | '\r' | "\r\n"
let digit = ['0'-'9']
let exp = 'e'['-''+']?['0'-'9']+
let double = (
    ((digit)+'.'(digit)* (exp)?) |
    ((digit)* '.'(digit)+(exp)?) |
    ((digit)+exp))
let singquote = '\''
let print_char = [' '-'~']


rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
(* single line comment starts with // and carries to end of line *)
| "//" [^'\n']* { tokenize lexbuf }
(* multi line comment starts with /*)
| "/*" { multicomment lexbuf }
| '(' { PAREN_L }
| ')' { PAREN_R }
| '{' { BRACE_L }
| '}' { BRACE_R }
| "[]" { SQB_PAIR }
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
| "~" { TWIDDLE }
| "class" { CLASS }
| "void" { VOID }
| "array"  { ARRAY }
| "length" { LENGTH }
| "bool" { BOOL }
| "char" { CHAR }
| "int" { INT }
| "double" { DOUBLE }
| "string" {STRING}
| "if" { IF }
| "else" { ELSE }
| "for" { FOR }
| "while" { WHILE }
| "return" { RETURN }
| "true" { TRUE }
| "false" { FALSE }
(* | "\"" ([^'\"']+ as str) "\"" { STRING_LITERAL(str) } *)
| ['0'-'9']+ as int { INT_LITERAL(int_of_string int) }
| double as doublelit {DOUBLE_LITERAL(float_of_string doublelit)}
| ['a'-'z''_']['A'-'Z''a'-'z''_''0'-'9']* as str { IDENTIFIER(str) }
| ['A'-'Z']['a'-'z''A'-'Z''_''0'-'9']*  as classLit { CLASS_IDENTIFIER(classLit) }
| '"'   { STRING_LITERAL(parse_string (Buffer.create 100) lexbuf) }
| singquote print_char singquote as char   {CHAR_LITERAL(char.[1])}
| eof { EOF }
| _ as char     { raise (Failure("Undefined character: " ^ Char.escaped char)) }

and parse_string buffer = parse
  '"'                 { Buffer.contents buffer }
| newline             { Buffer.add_string buffer (Lexing.lexeme lexbuf); parse_string buffer lexbuf }
| [^ '"'  '\n' '\r']+ { Buffer.add_string buffer (Lexing.lexeme lexbuf); parse_string buffer lexbuf }
| eof                 { raise (Failure("Non-terminated double quotes")) }

and multicomment = parse
"*/" { tokenize lexbuf }
| '\n' { Lexing.new_line lexbuf; multicomment lexbuf }
| eof { raise (Failure("reached end of file with an unclosed multiline comment"))}
| _ { multicomment lexbuf }
