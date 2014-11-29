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

%right ASSIGN		  
%nonassoc LE LT EQ
%left PLUS MINUS
%left MULT DIV
%left ISVOID
%left NEG
%left AT
%left DOT 

(*%start <Cool.posexpr list> exprtop*)
%start <Cool.posnode> program
%%

(* need at least one class. *)
program:
  | classes = classrule+; EOF { (Cool.Prog(classes), $endpos) }
;

classrule:
  | CLASS classname = TYPEID inh
    = preceded(INHERITS, TYPEID)? LBRACE features
    = classfield* RBRACE SEMI
	     { let inherits = (match inh with None -> "Object" | Some (x)  -> x ) in 
	       (Cool.Class { classname; inherits; features }, $endpos) }
;

(* todo: parses list, returns list  *)
classfield:
  | field = vardec SEMI { (Cool.VarField field, $endpos) }
  | methodname  = OBJECTID; LPAREN; formalparams = separated_list(COMMA, formal);
    RPAREN COLON returnType = TYPEID LBRACE defn
		       = posexpr RBRACE; SEMI;  { (Cool.Method { methodname; 
							 formalparams;
							 returnType;
							 defn }, $endpos) }
;

vardec:
  | fieldname = OBJECTID; COLON; fieldtype = TYPEID; 
    init = preceded(ASSIGN, posexpr)?; { { Cool.fieldname; Cool.fieldtype;
						Cool.init } }

formal:
  | id = OBJECTID COLON typ = TYPEID { (Cool.Formal(id, typ), $endpos) }
;

posexpr:
  | e = expr { (e, $endpos) }
;
expr:
  | LBRACE sub=nonempty_list(terminated(posexpr, SEMI)) RBRACE { Block (sub) }
  | LET; decls = separated_nonempty_list(COMMA, vardec);
    IN expr = posexpr { Cool.Let {decls; expr} }
  | IF pred=posexpr THEN thenexp=posexpr ELSE elseexp
    = posexpr FI { Cool.If { pred; thenexp; elseexp } }
  | NEW s = TYPEID { Cool.New(s) }
  | WHILE cond=posexpr LOOP body
    =posexpr POOL { Cool.Loop { cond; body }}
  | CASE test=posexpr OF branches
    =nonempty_list(terminated(branch, SEMI))
		  ESAC { Case {test; branches }} 
  | id = id; ASSIGN; e2 = posexpr %prec ASSIGN { Cool.Assign(id, e2) } 
  | NOT; e = posexpr %prec NOT { Cool.Comp(e) } 
  | e1 = posexpr; LE; e2 = posexpr %prec LE { Lequal(e1, e2) } 
  | e1 = posexpr; LT; e2 = posexpr  %prec LT { Less(e1, e2) } 
  | e1 = posexpr; EQ; e2 = posexpr %prec EQ { Equal(e1, e2) } 
  | e1 = posexpr; PLUS; e2 = posexpr %prec PLUS { Cool.Plus(e1, e2) } 
  | e1 = posexpr; MINUS; e2 = posexpr %prec MINUS { Cool.Minus(e1, e2) } 
  | e1 = posexpr; MULT; e2 = posexpr %prec MULT { Cool.Mult(e1, e2) } 
  | e1 = posexpr; DIV; e2 = posexpr %prec DIV { Cool.Div(e1, e2) } 
  | ISVOID; e = posexpr %prec ISVOID { Cool.IsVoid(e) } 
  | NEG; e = posexpr %prec NEG  { Cool.Neg(e) } 
  (* | obj = posexpr; dispatchType = option(preceded(AT, TYPEID)); DOT; *)
  (*   ide = id; LPAREN; args = separated_list(COMMA, posexpr); *)
  (*   RPAREN  { Dispatch { obj; dispatchType; id=ide.name; args } } *)
  | LPAREN; e = expr; RPAREN { e }
  | int = INT_CONST { Cool.Int(int) } 
  | str = STR_CONST { Cool.Str(str) } 
  | b = BOOL_CONST { Cool.Bool(b) } 
  | name = id { Cool.Id name }

id:
  | name = OBJECTID { { Cool.name; Cool.typ=None }}
branch:
  | branchname=OBJECTID COLON branchtype=TYPEID DARROW branche
    =posexpr { { branchname; branchtype; branche } }
(* expr[@TYPE].ID( [ expr [[, expr]] ∗ ] ) *) 
(* should be left associative *)
