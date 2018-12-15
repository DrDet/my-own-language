%{
  open Tree;;
%}

%token <string> STR_NAME
%token <string> NUM
%token <string> STR_LITERAL
%token TRUE
%token FALSE

%token INT LONG FLOAT BOOL STRING VOID
%token FUN
%token RET
%token FOR WHILE
%token IF ELSE ELSE_IF

%token PLUS MINUS MUL DIV MOD AND OR NOT
%token LOWER GREATER GREATER_EQ LOWER_EQ EQ NON_EQ ASSIGN PLUS_ASSIGN MINUS_ASSIGN

%token ROUND_BR_OPEN ROUND_BR_CLOSE
%token SQUARE_BR_OPEN SQUARE_BR_CLOSE
%token CURLY_BR_OPEN CURLY_BR_CLOSE

%token COLON SEMICOLON COMMA

%token EOF

%start main
%type <Tree.tree> main
%%

main:
    Code EOF        { $1 }

Code:
    GlobalStatement Code          { GlobalStatementsList($1, $2) }
    |                             { Eps }

GlobalStatement:
    VarDecl SEMICOLON        { GlobalVarDecl($1) }
    | VarDef SEMICOLON       { GlobalVarDef($1) }
    | Function               { $1 }
    | FuncCall SEMICOLON     { GlobalFuncCall($1) }

VarDecl:
	STR_NAME COLON Type      { VarDecl($1, $3) }

VarDef:
    STR_NAME COLON Type ASSIGN Exp  { VarDef($1, $3, $5) }

Function:
	FUN STR_NAME ROUND_BR_OPEN FunctionArgs ROUND_BR_CLOSE
    COLON TypeReturn
    CURLY_BR_OPEN Body CURLY_BR_CLOSE    { Function($2, $4, $7, $9) }

FunctionArgs:
	STR_NAME COLON Type FunctionArgsCont     { ArgList(Arg($1, $3), $4) }
    |                                        { NilArgList }

FunctionArgsCont:
	COMMA STR_NAME COLON Type FunctionArgsCont  { ArgList(Arg($2, $4), $5) }
    |                                           { NilArgList }

Body:
	LocalStatement Body      { LocalStatementsList($1, $2) }
    |                        { EpsBody }

LocalStatement:
	VarDecl SEMICOLON              { LocalVarDecl($1) }
    | VarDef SEMICOLON              { LocalVarDef($1) }
    | Cycle                        { $1 }
    | If                           { $1 }
    | Exp SEMICOLON                { LocalExp($1) }
    | RET Exp SEMICOLON            { Ret($2) }
    | RET SEMICOLON                { RetEmpty }

Exp:
    Exp ASSIGN S	         { BinaryOp("=", $1, $3) }
    | Exp PLUS_ASSIGN S	     { BinaryOp("+=", $1, $3) }
    | Exp MINUS_ASSIGN S	 { BinaryOp("-=", $1, $3) }
    | S                          { $1 }

S:
    S OR D	 { BinaryOp("||", $1, $3) }
    | D      { $1 }

D:
    D AND F	 { BinaryOp("&&", $1, $3) }
    | F      { $1 }

F:
    F EQ G	            { BinaryOp("==", $1, $3) }
    | F NON_EQ G	    { BinaryOp("!=", $1, $3) }
    | G                 { $1 }

G:
    G LOWER H	           { BinaryOp("<", $1, $3) }
    | G LOWER_EQ H	       { BinaryOp("<=", $1, $3) }
    | G GREATER H	       { BinaryOp(">", $1, $3) }
    | G GREATER_EQ H	   { BinaryOp(">=", $1, $3) }
    | H                    { $1 }

H:
    H PLUS J	    { BinaryOp("+", $1, $3) }
    | H MINUS J	    { BinaryOp("-", $1, $3) }
    | J             { $1 }

J:
    J MUL K	         { BinaryOp("*", $1, $3) }
    | J DIV K	     { BinaryOp("/", $1, $3) }
    | J MOD K	     { BinaryOp("%", $1, $3) }
    | K              { $1 }

K:
    PLUS L	         { UnaryOp("+", $2) }
    | MINUS L	     { UnaryOp("-", $2) }
    | NOT L          { UnaryOp("!", $2) }
    | L              { $1 }

L:
    ROUND_BR_OPEN Exp ROUND_BR_CLOSE	       { $2 }
    | STR_NAME	                               { Str_name($1) }
    | NUM	                                   { Num($1) }
    | TRUE	                                   { True }
    | FALSE	                                   { False }
    | FuncCall                                 { ExpFuncCall($1) }
    | STR_NAME TypeArrayAccess                 { ArrayAccess($1, $2) }
    | STR_LITERAL                              { StrLiteral($1) }

FuncCall:
    STR_NAME ROUND_BR_OPEN ExpList ROUND_BR_CLOSE       { FuncCall($1, $3) }

ExpList:
    Exp ExpListCont	 { ExpList($1, $2) }
    |                { NilExpList }

ExpListCont:
    COMMA Exp ExpListCont	 { ExpList($2, $3) }
    |                        { NilExpList }

Cycle:
    For 	     { $1 }
    | While      { $1 }

For:
    FOR ROUND_BR_OPEN VarDef SEMICOLON
    Exp SEMICOLON Exp ROUND_BR_CLOSE
    CURLY_BR_OPEN Body CURLY_BR_CLOSE                { For($3, $5, $7, $10) }

While:
    WHILE ROUND_BR_OPEN Exp ROUND_BR_CLOSE CURLY_BR_OPEN Body CURLY_BR_CLOSE     { While($3, $6) }

If:
    IF ROUND_BR_OPEN Exp ROUND_BR_CLOSE CURLY_BR_OPEN Body CURLY_BR_CLOSE IfCont    { If($3, $6, $8) }

IfCont:
    ELSE_IF ROUND_BR_OPEN Exp ROUND_BR_CLOSE CURLY_BR_OPEN Body CURLY_BR_CLOSE IfCont	 { ElseIf($3, $6, $8) }
    | ELSE CURLY_BR_OPEN Body CURLY_BR_CLOSE                                             { Else($3) }
    |                                                                                    { EpsIfCont }

Type:
    TypeBasic TypeArray     { Type($1, $2) }
    | TypeBasic             { Type($1, EpsTArr) }

TypeBasic:
    INT	     { Int }
    | LONG	 { Long }
    | FLOAT	 { Float }
    | BOOL	 { Bool }
    | STRING { String }

TypeArray:
    SQUARE_BR_OPEN NUM SQUARE_BR_CLOSE TypeArrayCont    { TypeArray($2, $4) }

TypeArrayCont:
    SQUARE_BR_OPEN NUM SQUARE_BR_CLOSE TypeArrayCont    { TypeArray($2, $4) }
    |                                                   { EpsTArr }

TypeReturn:
    TypeBasic	 { Type($1, EpsTArr) }
    | VOID       { Type(Void, EpsTArr) }

TypeArrayAccess:
    SQUARE_BR_OPEN Exp SQUARE_BR_CLOSE TypeArrayAccessCont    { TypeArrayAccess($2, $4) }

TypeArrayAccessCont:
    SQUARE_BR_OPEN Exp SQUARE_BR_CLOSE TypeArrayAccessCont    { TypeArrayAccess($2, $4) }
    |                                                         { EpsTArrAccess }
