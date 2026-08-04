%{    
    open Ast
%}

%token <string> VAR
%token INT_TYPE BOOL_TYPE FLOAT_TYPE VECTORI_TYPE VECTORF_TYPE MATRIXI_TYPE  MATRIXF_TYPE
%token PRINT INPUT
%token <string> STRING
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token PLUS SUB MUL DIV
%token EQ GT LT GEQ LEQ NEQ
%token AND OR NOT
%token LOOP WHILE DO DONE IF THEN ELSE
%token LPREN RPREN LSQU RSQU LBRA RBRA COMMA SEMICOL ASSGN
%token DIM ABS MOD MAG SQRT ANGLE TRACE TRANSMAT DET INVERSE
%token <Ast.expr list> VECTORI VECTORF
%token <Ast.expr list> MATRIXI MATRIXF
%token DOLL
%token EOF
%token ERROR


%left OR
%left AND
%nonassoc EQ NEQ
%nonassoc GT LT GEQ LEQ
%left PLUS SUB
%left MUL DIV MOD
%right NOT
%nonassoc INT
%nonassoc VECTORI VECTORF MATRIXI MATRIXF
%nonassoc COMMA

%start program
%type <Ast.stmt> program

%%

program:
    stmt_list EOF       { Block $1 }
  ;

  stmt_list:
    /* empty */         { [] }
  | stmt SEMICOL stmt_list { $1 :: $3 }
  | stmt                { [$1] }
  ;
         
  stmt:
    VAR ASSGN expr      { Assign($1, $3) }
  | PRINT LPREN expr RPREN { Print($3) }
  | INT_TYPE VAR ASSGN INPUT LPREN RPREN   { InputAssign(TInt, $2) }
  | FLOAT_TYPE VAR ASSGN INPUT LPREN RPREN   { InputAssign(TFloat, $2) }
  | BOOL_TYPE VAR ASSGN INPUT LPREN RPREN   { InputAssign(TBool, $2) }
  | VECTORI_TYPE VAR ASSGN INPUT LPREN RPREN   { InputAssign(TVectorI, $2) }
  | VECTORF_TYPE VAR ASSGN INPUT LPREN RPREN   { InputAssign(TVectorF, $2) }
  | MATRIXI_TYPE VAR ASSGN INPUT LPREN RPREN   { InputAssign(TMatrixI, $2) }
  | MATRIXF_TYPE VAR ASSGN INPUT LPREN RPREN   { InputAssign(TMatrixF, $2) }
  | INPUT LPREN expr RPREN { Input (Some($3)) }
  | IF LPREN expr RPREN THEN LBRA stmt_list RBRA ELSE LBRA stmt_list RBRA { If($3, Block($7), Block($11)) }
  | WHILE LPREN expr RPREN DO LBRA stmt_list RBRA { While($3, Block($7)) }
  | LOOP LPREN stmt DOLL expr DOLL stmt RPREN DO LBRA stmt_list RBRA { Loop( $3, $5,$7, Block($11)) } 
  |LBRA stmt_list RBRA { Block($2) }
  | expr              { Expr $1 }
  | INT_TYPE VAR ASSGN expr    { Declaration(TInt, $2, $4) }
  | BOOL_TYPE VAR ASSGN expr   { Declaration(TBool, $2, $4) }
  | FLOAT_TYPE VAR ASSGN expr  { Declaration(TFloat, $2, $4) }
  | VECTORI_TYPE VAR ASSGN expr { Declaration(TVectorI, $2, $4) }
  | VECTORF_TYPE VAR ASSGN expr { Declaration(TVectorF, $2, $4) }
  | MATRIXI_TYPE VAR ASSGN expr { Declaration(TMatrixI, $2, $4) }
  | MATRIXF_TYPE VAR ASSGN expr { Declaration(TMatrixF, $2, $4) }
  ;                                     
                         
  expr:
  | expr PLUS expr      { DualOp("+", $1, $3) }
  | expr SUB expr       { DualOp("-", $1, $3) }
  | expr MUL expr       { DualOp("*", $1, $3) }
  | expr DIV expr       { DualOp("/", $1, $3) }
  | expr EQ expr        { DualOp("=", $1, $3) }
  | expr GT expr        { DualOp(">", $1, $3) }
  | expr LT expr        { DualOp("<", $1, $3) }
  | expr GEQ expr       { DualOp(">=", $1, $3) }
  | expr LEQ expr       { DualOp("<=", $1, $3) }
  | expr NEQ expr       { DualOp("!=", $1, $3) }
  | expr AND expr       { DualOp("&&", $1, $3) }
  | expr OR expr        { DualOp("||", $1, $3) }
  | expr MOD expr        { DualOp("%", $1, $3) }
  | NOT expr            { SinOp("!", $2) }
  | LPREN expr RPREN     { $2 }
  | STRING              { String $1 }
  
  | FLOAT               { Float $1 }
  | BOOL                { Bool $1 }
  | VAR                 { Var $1 }
  | dimension_sin VECTORI          { Vectori $2 }  // For int vectors
  | dimension_sin VECTORF          { Vectorf $2 }  // For int vectors 
  | dimension_dou MATRIXI { Matrixi $2 }
  | dimension_dou MATRIXF { Matrixf $2 }
  | DIM LPREN expr RPREN  { SinOp("dim", $3) }
  | ABS LPREN expr RPREN  { SinOp("abs", $3) }
  | MAG LPREN expr RPREN  { SinOp("mag", $3) }
  | ANGLE LPREN expr COMMA expr RPREN { DualOp("angle", $3 , $5) }
  | TRACE LPREN expr RPREN {SinOp("trace",$3) }
  | TRANSMAT LPREN expr RPREN { SinOp("transpose", $3) }
  | SQRT LPREN expr RPREN { SinOp("sqrt",$3)}
  | DET LPREN expr RPREN  { SinOp("det", $3) }
  | INVERSE LPREN expr RPREN { SinOp("inv",$3) }
  | INT                 { Int $1 }
  ;

  dimension_dou:
  | INT COMMA INT {($1,$3)}
  ;
  
  dimension_sin:
  | INT {$1}
  ;

%%

