%token ASSIGN
%token AT
%token CASE
%token CLASS
%token COLON
%token COMMA
%token DARROW
%token DIV
%token DOT
%token ELSE
%token EOF
%token EQ
%token ESAC
%token FI
%token IF
%token IN
%token INHERITS
%token ISVOID
%token LBRACE
%token LE
%token LET
%token LOOP
%token LPAREN
%token LT
%token MINUS
%token MULT
%token NEG
%token NEW
%token NOT
%token OF
%token PLUS
%token POOL
%token RBRACE
%token RPAREN
%token SEMI
%token THEN
%token WHILE 
%token <bool>  BOOL_CONST 
%token <string> INT_CONST 
%token <string> OBJECTID 
%token <string> STR_CONST 
%token <string> TYPEID 
%token <string> ERROR 

%start <Cool.node> program

%%

program:
  | v = INT_CONST { `Int(v) }
;
