%{
    open Cool;;
    open Cool_tools;;
    let obj = TypeId.obj;;
    type tvar = Cool.TypeId.tvar;;
%}

%token ASSIGN AT CASE CLASS COLON COMMA DARROW DIV DOT ELSE EOF EQ
       ESAC FI IF IN INHERITS ISVOID LBRACE LE LET LOOP LPAREN LT OF
       MINUS MULT NEG NEW NOT PLUS POOL RBRACE RPAREN SEMI THEN WHILE 
%token <string> INT_CONST  STR_CONST   ERROR 
%token <Cool.ObjId.id>  OBJECTID
%token <Cool.TypeId.tvar> TYPEID
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

(*
error recovery:

- error in a class definition but the class is terminated
  properly and the next class is syntactically correct, the
  parser should be able to restart at the next class definition

- from errors in features (going on to the next feature)

- a let binding going on to the next variable)

- an expression inside a {...} block
*)

%start <Cool.posprog> program
(*%start <Cool.posexpr> posexpr*)

%%
program:
  | classes = classlists; EOF { (classes, (convert $endpos(classes))) }
  | ERROR { (raise (Cool.ParseError (convert $endpos)), (convert $endpos))} (* to shush a compiler *)
				(* warning *)

classlists:
  | rev = revclasslists { List.rev rev }
 
revclasslists:
  | CLASS c = singleclass { c :: [] }
  | rest = revclasslists CLASS cl = singleclass  { cl :: rest }
  | rest = revclasslists CLASS e = error  {
			  syntax_error (convert $startpos(e)) $startofs(e) "classlist";
			  (raise (Cool.ParseError (convert $endpos)), (convert $endpos)) :: []
			}
  | CLASS e = error { syntax_error (convert $startpos(e)) $startofs(e) "classlist";
			      (raise (Cool.ParseError (convert $endpos)), (convert $endpos)) :: [] }

singleclass:
  | classname = TYPEID inherits = preceded(INHERITS, TYPEID)? LBRACE features
    = classfield* RBRACE SEMI
	     { let open Core.Std in let methods = List.filter_map features ~f:(function | (Cool.ParserMethod(mr), pos) -> Some(mr, pos) | _ -> None) in 
					let fields = List.filter_map features ~f:(function | (Cool.ParserField(mf), pos) -> Some (mf,pos) | _ -> None) in
					let classname = (match classname with 
					| TypeId.Absolute(t) -> t
					| _ -> failwith "classname must not be self") in
					let inherits = (match inherits with 
					| Some(TypeId.Absolute(t)) -> t
					| None -> TypeId.objt
					| _ -> failwith "invalid inherits")  
				in ( { classname;  inherits; methods; fields}, (convert $endpos))  }

methodid:
  | nam = OBJECTID { MethodId.t_of_objid nam }

classfield:
  | field = vardec SEMI { (Cool.ParserField field, (convert $endpos)) }
  | methodname  = methodid; LPAREN; formalparams = separated_list(COMMA, formal);
    RPAREN COLON returnType = TYPEID LBRACE defn
		       = posexpr RBRACE; SEMI
    ;  { (Cool.ParserMethod { methodname; formalparams; returnType; defn },  (convert $endpos)) }
  | error SEMI {
	    syntax_error (convert $startpos) $startofs "classfield";
			      (raise (ParseError (convert $endpos)), (convert $endpos)) }
  | error EOF { failwith "save me" }

posexpr:
  | expr = expr { (untyped_expr expr (convert $endpos)) }

vardec:
  | fieldname = OBJECTID COLON fieldtype = TYPEID init=preceded(ASSIGN, posexpr)?; {
			let fieldname = (match fieldname with | Cool.ObjId.Name(t) -> t | _ -> failwith "declaring name") in  
		     { fieldname; fieldtype; init; } }
			
formal:
  | id = OBJECTID COLON typ = TYPEID { 
		let id=(match id with | Cool.ObjId.Name(t) -> t| _ -> failwith "id in formal") in
		let typ=(match typ with | Cool.TypeId.Absolute(t) -> t | _ -> failwith "type id in  a formal") in	
		((id, typ), (convert $endpos)) }


revdecls:
  | dec = vardec { dec :: [] }
  | rest = revdecls COMMA dec = vardec { dec :: rest }
  | e = error { e; syntax_error (convert $startpos(e)) $startofs(e) "revdecls"; {fieldname=failwith "error"; fieldtype =obj; init=(Some ( untyped_expr  ExprError (convert $endpos)))} :: []  }
  | rest = revdecls COMMA e = error 
				{ e; syntax_error (convert $startpos(e))
				     $startofs(e) "many"; 
				  {fieldname=failwith "error "; fieldtype=obj; init=(Some (untyped_expr ExprError (convert $endpos)))} :: []  }
  | error EOF { failwith "save me" }

letdecls:
  | rev = revdecls { List.rev rev }

revbrace:
  | first = posexpr SEMI { first :: [] }
  | rest = revbrace first = posexpr SEMI { first :: rest }
  | error SEMI { syntax_error (convert $startpos) $startofs "withinbrace";  (untyped_expr ExprError  (convert $endpos)) :: [] }
  | error EOF { failwith "save me" }

brace:
  | revbr = revbrace { List.rev revbr }

within:
  | expr = posexpr %prec LET { expr }
  | error { syntax_error (convert $startpos) $startofs "within"; (untyped_expr ExprError (convert $endpos)) }
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
  | id = id; ASSIGN; e2 = posexpr %prec ASSIGN { 
		let id=(match id with | Cool.ObjId.Name(t)-> t| _ -> failwith "assign id") in
		Cool.Assign(id, e2) } 
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
  | id = methodid; LPAREN; args = separated_list(COMMA, posexpr); 
    RPAREN { Dispatch { Cool.obj=(untyped_expr (Cool.Id ObjId.Self) (convert $startpos(id))); 
			 Cool.dispatchType=None;
			 Cool.id; args } } 
  | obj = posexpr;  DOT;  
     ide = id; LPAREN; args = separated_list(COMMA, posexpr); 
     RPAREN { Dispatch { Cool.obj; Cool.dispatchType=None;
				    Cool.id=(MethodId.t_of_objid ide); args } } 
  | obj = posexpr AT distype = TYPEID DOT ide = id; LPAREN; 
    args = separated_list(COMMA, posexpr) RPAREN
	{ let dispatchType = (match distype with | TypeId.Absolute(t) -> Some(t) | _ -> failwith "dispatch type" ) in
		 Dispatch { Cool.obj; dispatchType; Cool.id=(MethodId.t_of_objid ide); args } } 
  | LPAREN; e = expr; RPAREN { e }
  | int = INT_CONST { Cool.Int(int) } 
  | str = STR_CONST { Cool.Str(str) } 
  | b = BOOL_CONST { Cool.Bool(b) } 
  | name = id { Cool.Id name }

id:
  | name = OBJECTID { name }
branch:
  | branchname=OBJECTID COLON branchtype=TYPEID DARROW branche
    =posexpr { let branchtype = (match branchtype with | TypeId.Absolute(t) -> t | _ -> failwith "branchtype") in
		let branchname = (match branchname with | ObjId.Name(t) -> t | _ -> failwith "branchname") in
		{ branchname; branchtype; branche } }
