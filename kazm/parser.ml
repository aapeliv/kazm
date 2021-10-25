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

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
 open Ast 
# 59 "parser.ml"
let yytransl_const = [|
  257 (* FROM *);
  258 (* IMPORT *);
  259 (* PAREN_L *);
  260 (* PAREN_R *);
  261 (* BRACE_L *);
  262 (* BRACE_R *);
  263 (* SQB_L *);
  264 (* SQB_R *);
  265 (* DOT *);
  266 (* SEMI *);
  267 (* COMMA *);
  268 (* MOD *);
  269 (* ASSIGN *);
  270 (* PLUS *);
  271 (* MINUS *);
  272 (* TIMES *);
  273 (* DIVIDE *);
  274 (* PLUSEQ *);
  275 (* MINUSEQ *);
  276 (* TIMESEQ *);
  277 (* DIVIDEEQ *);
  278 (* PLUSPLUS *);
  279 (* MINUSMINUS *);
  280 (* AND *);
  281 (* OR *);
  282 (* NOT *);
  283 (* EQ *);
  284 (* NEQ *);
  285 (* LT *);
  286 (* LEQ *);
  287 (* GT *);
  288 (* GEQ *);
  289 (* EMPTY *);
  290 (* VOID *);
  291 (* BOOL *);
  292 (* CHAR *);
  293 (* INT *);
  294 (* DOUBLE *);
  295 (* IF *);
  296 (* THEN *);
  297 (* ELSE *);
  298 (* ELSIF *);
  299 (* FOR *);
  300 (* WHILE *);
  301 (* DO *);
  302 (* RETURN *);
  303 (* BREAK *);
  304 (* CONTINUE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  305 (* STRING_LITERAL *);
  306 (* INT_LITERAL *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\004\000\006\000\
\006\000\005\000\010\000\011\000\011\000\011\000\012\000\012\000\
\012\000\012\000\012\000\013\000\014\000\017\000\015\000\016\000\
\009\000\009\000\008\000\018\000\018\000\007\000\019\000\019\000\
\019\000\019\000\019\000\019\000\019\000\019\000\019\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\001\000\001\000\001\000\004\000\003\000\
\001\000\005\000\003\000\003\000\002\000\001\000\001\000\001\000\
\001\000\001\000\001\000\002\000\007\000\003\000\003\000\004\000\
\003\000\001\000\002\000\003\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\031\000\000\000\000\000\000\000\000\000\
\002\000\040\000\000\000\004\000\005\000\006\000\000\000\000\000\
\030\000\000\000\009\000\000\000\000\000\000\000\000\000\001\000\
\003\000\000\000\027\000\000\000\000\000\036\000\037\000\038\000\
\039\000\026\000\000\000\007\000\008\000\000\000\000\000\000\000\
\010\000\025\000\015\000\000\000\000\000\000\000\000\000\014\000\
\016\000\017\000\018\000\019\000\000\000\020\000\000\000\000\000\
\011\000\000\000\000\000\029\000\000\000\023\000\012\000\000\000\
\024\000\000\000\000\000\028\000\000\000\021\000"

let yydgoto = "\002\000\
\010\000\011\000\012\000\013\000\014\000\018\000\046\000\015\000\
\035\000\041\000\047\000\048\000\049\000\050\000\051\000\052\000\
\000\000\061\000\016\000"

let yysindex = "\009\000\
\001\000\000\000\214\254\000\000\015\255\034\255\035\255\036\255\
\000\000\000\000\006\000\000\000\000\000\000\000\020\255\214\254\
\000\000\003\255\000\000\037\255\038\255\039\255\040\255\000\000\
\000\000\251\254\000\000\214\254\214\254\000\000\000\000\000\000\
\000\000\000\000\016\255\000\000\000\000\019\255\251\254\225\254\
\000\000\000\000\000\000\041\255\225\254\006\255\028\255\000\000\
\000\000\000\000\000\000\000\000\225\254\000\000\214\254\225\254\
\000\000\225\254\033\255\000\000\017\255\000\000\000\000\044\255\
\000\000\214\254\225\254\000\000\029\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\001\255\002\255\004\255\005\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\030\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\044\000\000\000\000\000\000\000\253\255\234\255\
\000\000\000\000\245\255\214\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yytablesize = 300
let yytable = "\019\000\
\009\000\043\000\054\000\034\000\028\000\024\000\017\000\044\000\
\055\000\001\000\059\000\029\000\027\000\062\000\045\000\063\000\
\042\000\017\000\056\000\038\000\065\000\020\000\026\000\040\000\
\036\000\037\000\039\000\066\000\004\000\005\000\006\000\007\000\
\008\000\057\000\070\000\013\000\064\000\058\000\058\000\013\000\
\021\000\022\000\023\000\053\000\030\000\031\000\032\000\033\000\
\067\000\032\000\033\000\060\000\034\000\035\000\025\000\069\000\
\000\000\000\000\000\000\000\000\000\000\000\000\068\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\000\000\000\000\000\000\000\000\003\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\005\000\006\000\007\000\008\000\004\000\
\005\000\006\000\007\000\008\000"

let yycheck = "\003\000\
\000\000\033\001\045\000\026\000\002\001\000\000\049\001\039\001\
\003\001\001\000\053\000\009\001\016\000\056\000\046\001\058\000\
\039\000\049\001\013\001\004\001\004\001\007\001\003\001\005\001\
\028\000\029\000\011\001\011\001\034\001\035\001\036\001\037\001\
\038\001\006\001\006\001\006\001\004\001\010\001\010\001\010\001\
\007\001\007\001\007\001\003\001\008\001\008\001\008\001\008\001\
\005\001\049\001\049\001\055\000\049\001\049\001\011\000\067\000\
\255\255\255\255\255\255\255\255\255\255\255\255\066\000\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\255\255\255\255\255\255\255\255\001\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\034\001\035\001\036\001\037\001\038\001\034\001\
\035\001\036\001\037\001\038\001"

let yynames_const = "\
  FROM\000\
  IMPORT\000\
  PAREN_L\000\
  PAREN_R\000\
  BRACE_L\000\
  BRACE_R\000\
  SQB_L\000\
  SQB_R\000\
  DOT\000\
  SEMI\000\
  COMMA\000\
  MOD\000\
  ASSIGN\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  PLUSEQ\000\
  MINUSEQ\000\
  TIMESEQ\000\
  DIVIDEEQ\000\
  PLUSPLUS\000\
  MINUSMINUS\000\
  AND\000\
  OR\000\
  NOT\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  EMPTY\000\
  VOID\000\
  BOOL\000\
  CHAR\000\
  INT\000\
  DOUBLE\000\
  IF\000\
  THEN\000\
  ELSE\000\
  ELSIF\000\
  FOR\000\
  WHILE\000\
  DO\000\
  RETURN\000\
  BREAK\000\
  CONTINUE\000\
  EOF\000\
  "

let yynames_block = "\
  STRING_LITERAL\000\
  INT_LITERAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'blocks) in
    Obj.repr(
# 40 "parser.mly"
               ( Program(_1) )
# 319 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "parser.mly"
        ( Program("{empty program}") )
# 325 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'blocks) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 44 "parser.mly"
                 ( _1 ^ ";\n\n" ^ _2)
# 333 "parser.ml"
               : 'blocks))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 45 "parser.mly"
          ( _1 )
# 340 "parser.ml"
               : 'blocks))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'import_stmt) in
    Obj.repr(
# 48 "parser.mly"
                ( _1 )
# 347 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'func) in
    Obj.repr(
# 49 "parser.mly"
         ( _1 )
# 354 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'module_name) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 52 "parser.mly"
                                 ( "Importing " ^ _4 ^ " from " ^ _2 )
# 362 "parser.ml"
               : 'import_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'module_name) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 55 "parser.mly"
                         ( _1 ^ " . " ^ _3 )
# 370 "parser.ml"
               : 'module_name))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 56 "parser.mly"
         ( _1 )
# 377 "parser.ml"
               : 'module_name))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'dtype_with_name) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'arg_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'func_body) in
    Obj.repr(
# 59 "parser.mly"
                                                       (
      "Declared function " ^ _1 ^ " with arg list " ^ (List.fold_left (fun a b -> a ^ ", " ^ b) "" (List.rev _3)) ^ " and body: " ^ _5
    )
# 388 "parser.ml"
               : 'func))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 64 "parser.mly"
                          ( "stmts: " ^ (List.fold_left (fun a b -> a ^ "\n" ^ b) "" (List.rev _2)) )
# 395 "parser.ml"
               : 'func_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stmts) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 67 "parser.mly"
                    ( _3::_1 )
# 403 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 68 "parser.mly"
               ( _1 )
# 410 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 69 "parser.mly"
         ( _1::[] )
# 417 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
          ( "empty" )
# 423 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'return_stmt) in
    Obj.repr(
# 73 "parser.mly"
                ( _1 )
# 430 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'conditional_stmt) in
    Obj.repr(
# 74 "parser.mly"
                     ( _1 )
# 437 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assign_stmt) in
    Obj.repr(
# 75 "parser.mly"
                ( _1 )
# 444 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'call_stmt) in
    Obj.repr(
# 76 "parser.mly"
              ( _1 )
# 451 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 79 "parser.mly"
                ( "Return: " ^ _2 )
# 458 "parser.ml"
               : 'return_stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'stmt) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 82 "parser.mly"
                                                  ( "lone if with cond: " ^ _3 )
# 466 "parser.ml"
               : 'conditional_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'dtype_with_name) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 85 "parser.mly"
                                ( "assigning new var " ^ _3 ^ " to " ^ _1 )
# 474 "parser.ml"
               : 'decl_var_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 88 "parser.mly"
                     ( "assigning " ^ _3 ^ " to " ^ _1 )
# 482 "parser.ml"
               : 'assign_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'name_list) in
    Obj.repr(
# 91 "parser.mly"
                                   ( "calling " ^ _1 ^ " with name_list " ^ (List.fold_left (fun a b -> a ^ ", " ^ b) "" (List.rev _3)) )
# 490 "parser.ml"
               : 'call_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'dtype_with_name) in
    Obj.repr(
# 94 "parser.mly"
                                   ( _3::_1 )
# 498 "parser.ml"
               : 'arg_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'dtype_with_name) in
    Obj.repr(
# 95 "parser.mly"
                    ( _1::[] )
# 505 "parser.ml"
               : 'arg_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'dtype) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 98 "parser.mly"
               ( _2 ^ " (t: " ^ _1 ^ ")" )
# 513 "parser.ml"
               : 'dtype_with_name))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'name_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 101 "parser.mly"
                         ( _3::_1 )
# 521 "parser.ml"
               : 'name_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 102 "parser.mly"
         ( _1::[] )
# 528 "parser.ml"
               : 'name_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 105 "parser.mly"
                   ( _1 )
# 535 "parser.ml"
               : 'name))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "parser.mly"
         ( "void" )
# 541 "parser.ml"
               : 'dtype))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "parser.mly"
         ( "bool" )
# 547 "parser.ml"
               : 'dtype))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "parser.mly"
         ( "char" )
# 553 "parser.ml"
               : 'dtype))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser.mly"
        ( "int" )
# 559 "parser.ml"
               : 'dtype))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "parser.mly"
           ( "double" )
# 565 "parser.ml"
               : 'dtype))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "parser.mly"
                     ( "bool list" )
# 571 "parser.ml"
               : 'dtype))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "parser.mly"
                     ( "char list" )
# 577 "parser.ml"
               : 'dtype))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "parser.mly"
                    ( "int list" )
# 583 "parser.ml"
               : 'dtype))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "parser.mly"
                       ( "double list" )
# 589 "parser.ml"
               : 'dtype))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
