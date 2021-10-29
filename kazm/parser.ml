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
  | DIVIDEQ
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
# 57 "parser.ml"
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
  277 (* DIVIDEQ *);
  278 (* AND *);
  279 (* OR *);
  280 (* NOT *);
  281 (* EQ *);
  282 (* NEQ *);
  283 (* LT *);
  284 (* LEQ *);
  285 (* GT *);
  286 (* GEQ *);
  287 (* EMPTY *);
  288 (* VOID *);
  289 (* BOOL *);
  290 (* CHAR *);
  291 (* INT *);
  292 (* DOUBLE *);
  293 (* IF *);
  294 (* THEN *);
  295 (* ELSE *);
  296 (* ELSIF *);
  297 (* FOR *);
  298 (* WHILE *);
  299 (* DO *);
  300 (* RETURN *);
  301 (* BREAK *);
  302 (* CONTINUE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  303 (* STRING_LITERAL *);
  304 (* INT_LITERAL *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\004\000\006\000\
\006\000\005\000\010\000\011\000\011\000\011\000\012\000\012\000\
\012\000\012\000\012\000\013\000\014\000\017\000\015\000\016\000\
\009\000\009\000\019\000\019\000\019\000\019\000\019\000\020\000\
\020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\008\000\018\000\018\000\007\000\021\000\
\021\000\021\000\021\000\021\000\021\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\001\000\001\000\001\000\004\000\003\000\
\001\000\005\000\003\000\003\000\002\000\001\000\001\000\001\000\
\001\000\001\000\001\000\002\000\007\000\003\000\003\000\004\000\
\003\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\002\000\002\000\003\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\056\000\057\000\058\000\059\000\060\000\
\002\000\062\000\000\000\004\000\005\000\006\000\000\000\000\000\
\055\000\000\000\009\000\001\000\003\000\000\000\000\000\052\000\
\000\000\000\000\026\000\000\000\061\000\007\000\008\000\000\000\
\000\000\000\000\010\000\025\000\015\000\000\000\000\000\000\000\
\000\000\014\000\016\000\017\000\018\000\019\000\000\000\020\000\
\000\000\000\000\011\000\000\000\000\000\054\000\000\000\023\000\
\012\000\000\000\024\000\000\000\000\000\053\000\000\000\021\000"

let yydgoto = "\002\000\
\010\000\011\000\012\000\013\000\014\000\018\000\040\000\015\000\
\028\000\035\000\041\000\042\000\043\000\044\000\045\000\046\000\
\000\000\055\000\000\000\000\000\016\000"

let yysindex = "\004\000\
\001\000\000\000\217\254\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\006\000\000\000\000\000\000\000\008\255\251\254\
\000\000\012\255\000\000\000\000\000\000\250\254\032\255\000\000\
\217\254\217\254\000\000\020\255\000\000\000\000\000\000\031\255\
\250\254\228\254\000\000\000\000\000\000\038\255\228\254\007\255\
\027\255\000\000\000\000\000\000\000\000\000\000\228\254\000\000\
\217\254\228\254\000\000\228\254\039\255\000\000\021\255\000\000\
\000\000\040\255\000\000\217\254\228\254\000\000\028\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\029\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\033\000\000\000\000\000\000\000\253\255\241\255\
\000\000\000\000\242\255\221\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yytablesize = 298
let yytable = "\019\000\
\009\000\023\000\037\000\048\000\001\000\020\000\027\000\017\000\
\038\000\049\000\022\000\053\000\024\000\025\000\056\000\039\000\
\057\000\036\000\017\000\050\000\026\000\030\000\031\000\032\000\
\059\000\004\000\005\000\006\000\007\000\008\000\033\000\060\000\
\051\000\064\000\013\000\034\000\052\000\052\000\013\000\029\000\
\047\000\017\000\058\000\021\000\061\000\054\000\063\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\062\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
\004\000\005\000\006\000\007\000\008\000\004\000\005\000\006\000\
\007\000\008\000"

let yycheck = "\003\000\
\000\000\007\001\031\001\039\000\001\000\000\000\022\000\047\001\
\037\001\003\001\003\001\047\000\016\000\002\001\050\000\044\001\
\052\000\033\000\047\001\013\001\009\001\025\000\026\000\004\001\
\004\001\032\001\033\001\034\001\035\001\036\001\011\001\011\001\
\006\001\006\001\006\001\005\001\010\001\010\001\010\001\008\001\
\003\001\047\001\004\001\011\000\005\001\049\000\061\000\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\060\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
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
\032\001\033\001\034\001\035\001\036\001\032\001\033\001\034\001\
\035\001\036\001"

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
  DIVIDEQ\000\
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
# 316 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "parser.mly"
        ( Program("{empty program}") )
# 322 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'blocks) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 44 "parser.mly"
                 ( _1 ^ ";\n\n" ^ _2)
# 330 "parser.ml"
               : 'blocks))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 45 "parser.mly"
          ( _1 )
# 337 "parser.ml"
               : 'blocks))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'import_stmt) in
    Obj.repr(
# 48 "parser.mly"
                ( _1 )
# 344 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'func) in
    Obj.repr(
# 49 "parser.mly"
         ( _1 )
# 351 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'module_name) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 52 "parser.mly"
                                 ( "Importing " ^ _4 ^ " from " ^ _2 )
# 359 "parser.ml"
               : 'import_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'module_name) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 55 "parser.mly"
                         ( _1 ^ " . " ^ _3 )
# 367 "parser.ml"
               : 'module_name))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 56 "parser.mly"
         ( _1 )
# 374 "parser.ml"
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
# 385 "parser.ml"
               : 'func))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 64 "parser.mly"
                          ( "stmts: " ^ (List.fold_left (fun a b -> a ^ "\n" ^ b) "" (List.rev _2)) )
# 392 "parser.ml"
               : 'func_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stmts) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 67 "parser.mly"
                    ( _3::_1 )
# 400 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 68 "parser.mly"
               ( _1 )
# 407 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 69 "parser.mly"
         ( _1::[] )
# 414 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
          ( "empty" )
# 420 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'return_stmt) in
    Obj.repr(
# 73 "parser.mly"
                ( _1 )
# 427 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'conditional_stmt) in
    Obj.repr(
# 74 "parser.mly"
                     ( _1 )
# 434 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assign_stmt) in
    Obj.repr(
# 75 "parser.mly"
                ( _1 )
# 441 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'call_stmt) in
    Obj.repr(
# 76 "parser.mly"
              ( _1 )
# 448 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 79 "parser.mly"
                ( "Return: " ^ _2 )
# 455 "parser.ml"
               : 'return_stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'stmt) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 82 "parser.mly"
                                                  ( "lone if with cond: " ^ _3 )
# 463 "parser.ml"
               : 'conditional_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'dtype_with_name) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 85 "parser.mly"
                                ( "assigning new var " ^ _3 ^ " to " ^ _1 )
# 471 "parser.ml"
               : 'decl_var_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 88 "parser.mly"
                     ( "assigning " ^ _3 ^ " to " ^ _1 )
# 479 "parser.ml"
               : 'assign_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'name_list) in
    Obj.repr(
# 91 "parser.mly"
                                   ( "calling " ^ _1 ^ " with name_list " ^ (List.fold_left (fun a b -> a ^ ", " ^ b) "" (List.rev _3)) )
# 487 "parser.ml"
               : 'call_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'dtype_with_name) in
    Obj.repr(
# 94 "parser.mly"
                                   ( _3::_1 )
# 495 "parser.ml"
               : 'arg_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'dtype_with_name) in
    Obj.repr(
# 95 "parser.mly"
                    ( _1::[] )
# 502 "parser.ml"
               : 'arg_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "parser.mly"
           ( Assign )
# 508 "parser.ml"
               : 'assignment_operator))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "parser.mly"
           ( Pluseq )
# 514 "parser.ml"
               : 'assignment_operator))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "parser.mly"
            ( Minuseq )
# 520 "parser.ml"
               : 'assignment_operator))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "parser.mly"
            ( TimeseQ )
# 526 "parser.ml"
               : 'assignment_operator))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "parser.mly"
            ( Divideq )
# 532 "parser.ml"
               : 'assignment_operator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 105 "parser.mly"
                       ( Intlit(_1)               )
# 539 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 106 "parser.mly"
                       ( Stringlit(_1)            )
# 546 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "parser.mly"
                       ( Binop(_1, Add, _3)       )
# 554 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
                       ( Binop(_1, Sub, _3)       )
# 562 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                       ( Binop(_1, Mul, _3)       )
# 570 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "parser.mly"
                       ( Binop(_1, Div, _3)       )
# 578 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                       ( Binop(_1, Mod, _3)       )
# 586 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                       ( Binop(_1, Pluseq, _3)    )
# 594 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                       ( Binop(_1, Minuseq, _3)   )
# 602 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
                       ( Binop(_1, Timeseq, _3)   )
# 610 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                       ( Binop(_1, Divideq, _3)   )
# 618 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 116 "parser.mly"
                       ( Binop(_1, Equal, _3)     )
# 626 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "parser.mly"
                       ( Binop(_1, Neq, _3)       )
# 634 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                       ( Binop(_1, Less, _3)      )
# 642 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                       ( Binop(_1, Leq, _3)       )
# 650 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                       ( Binop(_1, Greater, _3)   )
# 658 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
                       ( Binop(_1, Geq, _3)       )
# 666 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                       ( Binop(_1, And, _3)       )
# 674 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                       ( Binop(_1, Or, _3)        )
# 682 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                       ( Unop(Not, _2)            )
# 689 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'dtype) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 129 "parser.mly"
               ( _2 ^ " (t: " ^ _1 ^ ")" )
# 697 "parser.ml"
               : 'dtype_with_name))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'name_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 132 "parser.mly"
                         ( _3::_1 )
# 705 "parser.ml"
               : 'name_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 133 "parser.mly"
         ( _1::[] )
# 712 "parser.ml"
               : 'name_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 136 "parser.mly"
                   ( _1 )
# 719 "parser.ml"
               : 'name))
; (fun __caml_parser_env ->
    Obj.repr(
# 139 "parser.mly"
         ( Void )
# 725 "parser.ml"
               : 'dtype))
; (fun __caml_parser_env ->
    Obj.repr(
# 141 "parser.mly"
         ( Bool )
# 731 "parser.ml"
               : 'dtype))
; (fun __caml_parser_env ->
    Obj.repr(
# 142 "parser.mly"
         ( Char )
# 737 "parser.ml"
               : 'dtype))
; (fun __caml_parser_env ->
    Obj.repr(
# 143 "parser.mly"
        ( Int )
# 743 "parser.ml"
               : 'dtype))
; (fun __caml_parser_env ->
    Obj.repr(
# 144 "parser.mly"
           ( Double )
# 749 "parser.ml"
               : 'dtype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'dtype) in
    Obj.repr(
# 146 "parser.mly"
                      ( Array(_1) )
# 756 "parser.ml"
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
