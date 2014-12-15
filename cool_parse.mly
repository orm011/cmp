%token ASSIGN AT CASE CLASS COLON COMMA DARROW DIV DOT ELSE EOF EQ
       ESAC FI IF IN INHERITS ISVOID LBRACE LE LET LOOP LPAREN LT OF
       MINUS MULT NEG NEW NOT PLUS POOL RBRACE RPAREN SEMI THEN WHILE 
%token <string> INT_CONST OBJECTID  STR_CONST  TYPEID ERROR 
%token <bool>  BOOL_CONST 

%nonassoc LET
%right ASSIGN
%right NOT
%nonassoc LE LT EQ
%left PLUS MINUS
%left MULT DIV
%left ISVOID
%left NEG
%left AT
%left DOT

%{
    open Cool;;
    open Cool_tools;;
(*      set_debug ();;*)
%}

(*
error recovery:

- error in a class definition but the class is terminated
  properly and the next class is syntactically correct, the
  parser should be able to restart at the next class definition

- from errors in features (going on to the next feature)

- a let binding going on to the next variable)

- an expression inside a {...} block
*)

%start <Cool.posnode> program
(*%start <Cool.posexpr> posexpr*)

%%
program:
  | classes = classlists; EOF { (Cool.Prog(classes), $endpos(classes)) }

classlists:
  | rev = revclasslists { List.rev rev }

revclasslists:
  | CLASS c = singleclass { c :: [] }
  | rest = revclasslists CLASS cl = singleclass  { cl :: rest }
  | rest = revclasslists CLASS e = error  {
			  syntax_error $startpos(e) $startofs(e) "classlist";
			  (ParseError, $endpos) :: []
			}
  | CLASS e = error { syntax_error $startpos(e) $startofs(e) "classlist";
			      (ParseError, $endpos) :: [] }

singleclass:
  | classname = TYPEID INHERITS inherits = TYPEID LBRACE features
    = classfield* RBRACE SEMI
	     { (Cool.Class { classname; inherits; features }, $endpos)  }
  | classname = TYPEID LBRACE features = classfield* RBRACE SEMI
	     { (Cool.Class { classname; inherits="Object"; features }, $endpos) }


classfield:
  | field = vardec SEMI { (Cool.VarField field, $endpos) }
  | methodname  = OBJECTID; LPAREN; formalparams = separated_list(COMMA, formal);
    RPAREN COLON returnType = TYPEID LBRACE defn
		       = posexpr RBRACE; SEMI;  { (Cool.Method { methodname; 
							 formalparams;
							 returnType;
							 defn }
						  , $endpos) }
  | error SEMI {
	    syntax_error $startpos $startofs "classfield";
			      (ParseError, $endpos) }
  | error EOF { failwith "save me" }

posexpr:
  | expr = expr { (untyped_expr expr $endpos) }

vardec:
  | fieldname = OBJECTID COLON fieldtype = TYPEID ASSIGN init=posexpr; {
		     { Cool.fieldname; Cool.fieldtype; Cool.init=init } }
  | fieldname = OBJECTID COLON fieldtype = TYPEID { { fieldname; fieldtype;
						    init=(untyped_expr NoExpr $endpos) }}
formal:
  | id = OBJECTID COLON typ = TYPEID { (Cool.Formal(id, typ), $endpos) }


revdecls:
  | dec = vardec { dec :: [] }
  | rest = revdecls COMMA dec = vardec { dec :: rest }
  | e = error { e; syntax_error $startpos(e) $startofs(e) "revdecls"; {fieldname="dummy"; fieldtype ="Dummy"; init=( untyped_expr  ExprError $endpos  )} :: []  }
  | rest = revdecls COMMA e = error 
				{ e; syntax_error $startpos(e)
				     $startofs(e) "many"; 
				  {fieldname="dummy"; fieldtype ="Dummy"; init=(untyped_expr ExprError $endpos)} :: []  }
  | error EOF { failwith "save me" }

letdecls:
  | rev = revdecls { List.rev rev }

revbrace:
  | first = posexpr SEMI { first :: [] }
  | rest = revbrace first = posexpr SEMI { first :: rest }
  | error SEMI { syntax_error $startpos $startofs "withinbrace";  (untyped_expr ExprError  $endpos) :: [] }
  | error EOF { failwith "save me" }

brace:
  | revbr = revbrace { List.rev revbr }

within:
  | expr = posexpr %prec LET { expr }
  | error { syntax_error $startpos $startofs "within"; (untyped_expr ExprError $endpos) }
  | error EOF { failwith "save me" }

expr:
  | LBRACE sub = brace RBRACE { Block (sub) }
  | LET; decls = letdecls IN letbody = within { 
					Cool.Let (Cool_tools.deflatten {decls;
							       letbody}
			     ) }
  | IF pred=posexpr THEN thenexp=posexpr ELSE elseexp
    = posexpr FI { Cool.If { pred; thenexp; elseexp } }
  | NEW s = TYPEID { Cool.New(s) }
  | WHILE cond=posexpr LOOP body
    =posexpr POOL { Cool.Loop { cond; body }}
  | CASE test=posexpr OF branches
    =nonempty_list(terminated(branch, SEMI))
		  ESAC { Case {test; branches }} 
  | id = id; ASSIGN; e2 = posexpr %prec ASSIGN { Cool.Assign(id, e2) } 
  | NOT; e = posexpr  { Cool.Comp(e) } 
  | e1 = posexpr; LE; e2 = posexpr %prec LE { Lequal(e1, e2) } 
  | e1 = posexpr; LT; e2 = posexpr  %prec LT { Lt(e1, e2) } 
  | e1 = posexpr; EQ; e2 = posexpr %prec EQ { Eq(e1, e2) } 
  | e1 = posexpr; PLUS; e2 = posexpr %prec PLUS { Cool.Plus(e1, e2) } 
  | e1 = posexpr; MINUS; e2 = posexpr %prec MINUS { Cool.Minus(e1, e2) } 
  | e1 = posexpr; MULT; e2 = posexpr %prec MULT { Cool.Mult(e1, e2) } 
  | e1 = posexpr; DIV; e2 = posexpr %prec DIV { Cool.Div(e1, e2) } 
  | ISVOID; e = posexpr %prec ISVOID { Cool.IsVoid(e) } 
  | NEG; e = posexpr %prec NEG  { Cool.Neg(e) } 
  | ide = id; LPAREN; args = separated_list(COMMA, posexpr); 
     RPAREN { Dispatch { Cool.obj=(untyped_expr (Cool.Id { name="self"; idtyp=None}) $startpos(ide)); 
			 Cool.dispatchType=None;
			 Cool.id=ide.Cool.name; args } } 
  | obj = posexpr;  DOT;  
     ide = id; LPAREN; args = separated_list(COMMA, posexpr); 
     RPAREN { Dispatch { Cool.obj; Cool.dispatchType=None;
				    Cool.id=ide.Cool.name; args } } 
  | obj = posexpr AT distype = TYPEID DOT ide = id; LPAREN; 
    args = separated_list(COMMA, posexpr) RPAREN
	{ Dispatch { Cool.obj; Cool.dispatchType=Some(distype); Cool.id=ide.Cool.name; args } } 
  | LPAREN; e = expr; RPAREN { e }
  | int = INT_CONST { Cool.Int(int) } 
  | str = STR_CONST { Cool.Str(str) } 
  | b = BOOL_CONST { Cool.Bool(b) } 
  | name = id { Cool.Id name }

id:
  | name = OBJECTID { { name; idtyp=None }}
branch:
  | branchname=OBJECTID COLON branchtype=TYPEID DARROW branche
    =posexpr { { branchname; branchtype; branche } }
