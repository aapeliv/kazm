type token =
  | FROM
  | IMPORT
  | PAREN_L
  | PAREN_R
  | BRACE_L
  | BRACE_R
  | SQB_L
  | SQB_R
  | DOT
  | SEMI
  | COMMA
  | MOD
  | ASSIGN
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | PLUSEQ
  | MINUSEQ
  | TIMESEQ
  | DIVIDEEQ
  | PLUSPLUS
  | MINUSMINUS
  | AND
  | OR
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | EMPTY
  | VOID
  | BOOL
  | CHAR
  | INT
  | DOUBLE
  | IF
  | THEN
  | ELSE
  | ELSIF
  | FOR
  | WHILE
  | DO
  | RETURN
  | BREAK
  | CONTINUE
  | STRING_LITERAL of (string)
  | INT_LITERAL of (int)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
