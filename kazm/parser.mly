/* Ocamlyacc parser for Kazm */
%{
open Ast
%}

%token PAREN_L PAREN_R BRACE_L BRACE_R SQB_L SQB_R SQB_PAIR /* ( ) { } [ ] */
%token DOT SEMI COMMA MOD ASSIGN TWIDDLE /* . ; , * % = ~ */
%token PLUS MINUS TIMES DIVIDE  /* + - * / */
%token PLUSEQ MINUSEQ TIMESEQ DIVIDEQ /* + - * / += -= *= /= */
%token AND OR NOT  /* && || ! */
%token EQ NEQ LT LEQ GT GEQ /* == != < <= > >= */
%token VOID BOOL CHAR INT DOUBLE STRING
%token IF ELSE FOR WHILE
%token RETURN BREAK
%token CLASS
%token ARRAY LENGTH
%token TRUE FALSE

%token<string> IDENTIFIER CLASS_IDENTIFIER
%token<string> CLASS_NAME
%token<string> STRING_LITERAL
%token<float> DOUBLE_LITERAL
%token<char> CHAR_LITERAL
%token<int> INT_LITERAL
/* %token<bool> BOOL_LITERAL */
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%nonassoc PAREN_L PAREN_R BRACE_L BRACE_R SQB_L SQB_R
%left SEMICO
%left IF
%right ASSIGN PLUSEQ MINUSEQ TIMESEQ DIVIDEQ
%left OR
%left AND
%left EQ NEQ
%left LT GT GEQ LEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT

%left DOT

%start program
%type <Ast.program> program
%%

program:
  decls EOF { $1 }

decls:
    /* nothing */ { ([], []) }
  | decls fdecl {
    let (f, s) = $1 in
    (f @ [$2], s)
    }
  | decls cdecl {
    let (f, s) = $1 in
    (f, s @ [$2])
    }

fdecl:
   typ IDENTIFIER PAREN_L formals_opt PAREN_R BRACE_L stmts BRACE_R
     { { typ = $1;
         fname = $2;
         formals = List.rev $4;
         body = $7 } }

cdecl:
    CLASS CLASS_IDENTIFIER BRACE_L class_body BRACE_R SEMI {
      let (vars, mthds, constructors, destructors) = $4 in
      { cname = $2; cvars = vars; cmethods = mthds; cconstructors = constructors; cdestructors = destructors; }
    }

class_body:
    { ([], [], [], []) }
  | class_body var_decl {
   let (f, s, t, h) = $1 in
   (f @ [$2], s, t, h)
  }
  | class_body mdecl {
   let (f, s, t, h) = $1 in
   (f, s @ [$2], t, h)
  }
  | class_body constructor_decl {
   let (f, s, t, h) = $1 in
   (f, s, t @ [$2], h)
  }
  | class_body destructor_decl {
   let (f, s, t, h) = $1 in
   (f, s, t, h @ [$2])
  }

mdecl:
   typ IDENTIFIER PAREN_L formals_opt PAREN_R BRACE_L stmts BRACE_R
     { { typ = $1;
         fname = $2;
         formals = List.rev $4;
         body = $7 } }

constructor_decl:
   CLASS_IDENTIFIER PAREN_L formals_opt PAREN_R BRACE_L stmts BRACE_R
     { { typ = Void;
         fname = $1;
         formals = List.rev $3;
         body = $6 } }

destructor_decl:
   TWIDDLE CLASS_IDENTIFIER PAREN_L PAREN_R BRACE_L stmts BRACE_R
     { { typ = Void;
         fname = $2;
         formals = [];
         body = $6 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }

formal_list:
    typ IDENTIFIER { [($1,$2)] }
  | formal_list COMMA typ IDENTIFIER { ($3,$4) :: $1 }

typ:
    VOID { Void }
  | BOOL { Bool }
  | CHAR { Char }
  | INT { Int }
  | DOUBLE { Double }
  | STRING { String }
  | CLASS_IDENTIFIER { ClassT($1) }
  | ARRAY typ SQB_L INT_LITERAL SQB_R {ArrT($2, $4)}

var_decl:
    typ IDENTIFIER SEMI { ($1, $2) } 

stmts:
    { [] }
  | stmts stmt { $1 @ [$2] }

stmt:
    expr SEMI { Expr $1 }
  | scope { $1 }
  | return_stmt SEMI { $1 }
  | break_stmt SEMI { $1 }
  | if_stmt { $1 }
  | while_stmt { $1 }
  | for_stmt { $1 }
  | var_decl_stmt SEMI { $1 }

scope:
    BRACE_L block_stmt BRACE_R { StmtScope($2) }

block_stmt:
    stmts { Block($1) }

return_stmt:
    RETURN expr { Return $2 }
  | RETURN { EmptyReturn }

break_stmt:
    BREAK { Break }

if_stmt:
    IF PAREN_L expr PAREN_R scope ELSE scope { If($3, $5, $7) }
  | IF PAREN_L expr PAREN_R scope { If($3, $5, Block([])) }

while_stmt:
    WHILE PAREN_L expr PAREN_R scope { While($3, $5) }

for_stmt:
    FOR PAREN_L expr SEMI expr SEMI expr PAREN_R scope { For($3, $5, $7, $9) }

var_decl_stmt:
    typ IDENTIFIER { Initialize(($1, $2), None) }
  | typ IDENTIFIER ASSIGN expr { Initialize(($1, $2), Some $4) }

expr:
    INT_LITERAL        { Literal($1) }
  | STRING_LITERAL     { StringLit($1) }
  | DOUBLE_LITERAL     { Dliteral(string_of_float $1)}
  | CHAR_LITERAL       { CharLit(Char.escaped $1)}
  | TRUE               { BoolLit(true) }
  | FALSE              { BoolLit(false) }
  | expr PLUS   expr   { Binop($1, Add,   $3)   }
  | expr MINUS  expr   { Binop($1, Sub,   $3)   }
  | expr TIMES  expr   { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr   { Binop($1, Div,   $3)   }
  | expr MOD expr      { Binop($1, Mod,   $3)   }
  | expr EQ     expr   { Binop($1, Equal, $3)   }
  | expr NEQ    expr   { Binop($1, Neq,   $3)   }
  | expr LT     expr   { Binop($1, Less,  $3)   }
  | expr LEQ    expr   { Binop($1, Leq,   $3)   }
  | expr GT     expr   { Binop($1, Greater, $3) }
  | expr GEQ    expr   { Binop($1, Geq,   $3)   }
  | expr AND    expr   { Binop($1, And,   $3)   }
  | expr OR     expr   { Binop($1, Or,    $3)   }
  | NOT expr           { Unop(Not, $2) }
  | MINUS expr %prec NOT { Unop(Neg, $2)      }
  | PAREN_L expr PAREN_R { $2 }
  | fq_identifier ASSIGN expr { Assign($1, $3) }
  | fq_identifier PAREN_L expr_list PAREN_R { Call($1, $3) }
  | fq_identifier      { Id($1) }
  | SQB_L expr_list SQB_R { ArrayLit($2) }
  | IDENTIFIER SQB_L expr SQB_R { ArrayAccess($1, $3) }
  | IDENTIFIER SQB_L expr SQB_R ASSIGN expr { ArrayAssign($1, $3, $6) }
  | IDENTIFIER DOT LENGTH { ArrayLength($1) }

fq_identifier:
    IDENTIFIER { [$1] }
  | IDENTIFIER DOT IDENTIFIER { $1::$3::[] }

expr_list:
    { [] }
  | expr_list COMMA expr { $1 @ [$3] }
  | expr { [$1] }
