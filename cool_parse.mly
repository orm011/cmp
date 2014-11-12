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
  | fieldname = OBJECTID; COLON; fieldtype = TYPEID;
    init = preceded(ASSIGN, posexpr)?; SEMI;
	     { (Cool.VarField { fieldname; fieldtype; init }, $endpos)}
  | methodname  = OBJECTID; LPAREN; formalparams = separated_list(COMMA, formal);
    RPAREN COLON returnType = TYPEID LBRACE defn
		       = posexpr RBRACE; SEMI;  { (Cool.Method { methodname; 
							 formalparams;
							 returnType;
							 defn }, $endpos) }
;

formal:
  | id = OBJECTID COLON typ = TYPEID { (Cool.Formal(id, typ), $endpos) }
;

(* using this temporary to test just the expressions component  *)
exprtop:
  | e = posexpr*; EOF {e}

posexpr:
  | e = expr { (e, $endpos) }
;
expr:
  | e = assignexpr { e }
;

(*
. level 0
@ level 1
~ level 2
isvoid level 3
* / level 4
+ - level 5
<= < = level 6
not level 7
<- level 8

All binary operations are left-associative, with the exception of
assignment, which is right-associative,
and the three comparison operations, which do not associate.
 *)

idpos:
  | id = idexpr { (id, $endpos) }
idexpr:
  | name = OBJECTID { Cool.Id {name; typ=None} }

assignpos:
  | e = assignexpr { (e, $endpos) }
(* ID <- expr *) (* right associative *)
assignexpr:
  | id = idpos; ASSIGN; e2 = assignpos { Cool.Assign(id, e2) }
  | e = complementexpr {e }

complementpos:
  | e = complementexpr { (e, $endpos ) }
;
complementexpr:
  | NOT; e = complementpos { Cool.Comp(e) }
  | e = compareexpr { e }
;

comparepos:
  | e = compareexpr { (e, $endpos) }
;

(* the three comparison operations do not associate, so you cannot *)
(* have a list of them*)
compareexpr:
  | e1 = sumpos; LE; e2 = sumpos  { Lequal(e1, e2) }
  | e1 = sumpos; LT; e2 = sumpos  { Less(e1, e2) }
  | e1 = sumpos; EQ; e2 = sumpos  { Equal(e1, e2) }
  | e = sumexpr { e }
;

sumpos:
  | e = sumexpr { (e, $endpos) }
;

sumexpr:
  | e1 = sumpos ; PLUS; e2 = prodpos { Cool.Plus(e1, e2) }
  | e1 = sumpos ; MINUS; e2 = prodpos { Cool.Minus(e1, e2) }
  | e = prodexpr { e }
;

prodpos:
  | e = prodexpr { (e, $endpos) }
;

prodexpr:
  | e1 = prodpos; MULT; e2 = isvoidpos { Cool.Mult(e1, e2) }
  | e1 = prodpos; DIV; e2 = isvoidpos { Cool.Div(e1, e2) }
  | e = isvoidexpr { e }
;

isvoidpos:
  | e = isvoidexpr { (e, $endpos) }
;
isvoidexpr:
  | ISVOID; e = isvoidpos { Cool.IsVoid(e) }
  | e = negexpr { e }
;

negpos:
  | e = negexpr { (e, $endpos) }
;

negexpr:
  | NEG; e = negpos { Cool.Neg(e) } (* this makes neg right associative *)
			       (* that's more useful, but contradicts *)
			       (* what the assignment seems to say. *)
  | e = dispatchexpr { e }
;

(* expr[@TYPE].ID( [ expr [[, expr]] ∗ ] ) *) 
(* should be left associative *)
dispatchpos:
  | e = dispatchexpr { (e, $endpos) }
;

dispatchexpr:
  | obj = dispatchpos; dispatchType = option(preceded(AT, TYPEID)); DOT;
    ide = idexpr; LPAREN; args = separated_list(COMMA, posexpr);
    RPAREN  { Dispatch { obj; dispatchType;
			 id=(match ide with Cool.Id(r) -> r.name |
					    _ -> failwith "only id"); args } }
  | e = basicexpr { e }
;

basicpos:
  | e = basicexpr { (e, $endpos) }
;
basicexpr:
  | LPAREN; e = expr; RPAREN { e }
  | id = idexpr { id }
  | int = INT_CONST { Cool.Int(int) } 
  | str = STR_CONST { Cool.Str(str) } 
  | b = BOOL_CONST { Cool.Bool(b) } 
;
