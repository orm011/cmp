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

%start <Cool.posnode> program

%%

(* need at least one class. *)
program:
  | classes = classrule+; EOF { (Cool.Prog(classes), $endpos) }
;

classrule:
  | CLASS classname = TYPEID inh
    = preceded(INHERITS, TYPEID)? LBRACE features
    = fields RBRACE SEMI
	     { let inherits = (match inh with None -> "Object" | Some (x)  -> x ) in 
	       (Cool.Class { classname; inherits; features }, $endpos
	     ) }
;

fields:
  | { [] }
  | fl = classfield; SEMI; rest = fields { fl :: rest }
  (* | obj = separated_list(SEMI, classfield) { obj } *)
;

(* todo: parses list, returns list  *)
classfield:
  | fieldname = OBJECTID; COLON; fieldtype = TYPEID; 
	     { (Cool.VarField { fieldname; fieldtype }, $endpos)}
;
