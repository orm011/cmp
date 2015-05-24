open Core.Std;;


type lexpos = {
		fname : string;
   	lnum : int;
   	bol : int;
   	cnum : int;
		} with sexp

val convert : Lexing.position -> lexpos

module TypeId : sig
    type t with sexp (* cannot be self-type *)
    type tvar = Absolute of t | SelfType
    val tvar_of_string: string -> tvar
    val string_of_tvar: tvar -> string
    val t_of_tvar: tvar -> t
		val string_of_t: t -> string
    include Comparable.S with type t := t
    val obj : tvar
    val objt : t
    val intt : t
    val stringt : t
    val boolt : t
end

module ObjId : sig
    type t with sexp (* cannot be self *)
    include Comparable.S with type t := t
    type id = Name of t | Self
    val t_of_id: id -> t
		val string_of_t: t -> string
    val id_of_string: string -> id
    val string_of_id: id -> string
end

module MethodId : sig
    type t with sexp
    include Comparable.S with type t:= t
    val t_of_objid: ObjId.id -> t
    val string_of_t: t -> string
end

type formal = ObjId.t * TypeId.t


type expr =
  | Let of letrec
  | Assign of ObjId.t * posexpr
  | Comp of posexpr
  | Lequal of posexpr * posexpr
  | Lt of posexpr * posexpr
  | Eq of posexpr * posexpr
  | Plus of posexpr * posexpr
  | Minus of posexpr * posexpr
  | Mult of posexpr * posexpr
  | Div of posexpr * posexpr
  | IsVoid of posexpr
  | Neg of posexpr
  | Dispatch of dispatchrec
  | Id of ObjId.id
  | Int of string
  | Str of string
  | Bool of bool
  | Block of posexpr list
  | If of ifrec
  | New of TypeId.tvar (* can be self *)
  | Loop of looprec
  | Case of caserec
  | NoExpr
  | ExprError
and caserec = { test:posexpr; branches:branch list}
and branch  = { branchname:ObjId.t; branchtype:TypeId.t;  branche:posexpr }
and looprec =  { cond:posexpr; body:posexpr }
and ifrec = { pred:posexpr; thenexp:posexpr; elseexp:posexpr }
and letrec = { decls: fieldr list; letbody: posexpr }
and dispatchrec = {obj:posexpr; dispatchType:TypeId.t option; id:MethodId.t; args:posexpr list}
and posexpr = { 
	expr:expr;
	pos:lexpos;
	exprtyp:TypeId.tvar option (* it really can be SELF_TYPE *) 
	}
and fieldr = { 
	fieldname : ObjId.t; (* cannot be self *) 
	fieldtype : TypeId.tvar; (* can be self_type *) 
	init : posexpr;
} with sexp
		
type posfield = fieldr * lexpos
				
type methodr = { 
	methodname: MethodId.t; 
	formalparams: (formal * lexpos) list;  (* yes, formals with position are printed by reference *)
	returnType: TypeId.tvar; 
	defn:posexpr
	}
	
type posmethod = methodr * lexpos

(* used only for the parser *)
type feature = ParserMethod of methodr | ParserField of fieldr  

type cool_class = { 
	classname : TypeId.t; 
	inherits : TypeId.t; 
	methods : posmethod list; 
	fields : posfield list 
}
		
type posclass = cool_class * lexpos

type prog = posclass list
type posprog = prog * lexpos
 

exception ParseError of lexpos
(* the formal params is a list of formals with position
but to print them we need them to be posnodes *)


(* the nature of Self_Type: *)
(* okay in: 1) method return type 2) field declaration type 3) let declaration type  4) argument to new *)
(* What is the meaning of Self_Type in those cases above *)
(* not okay in 1) class name, 2) inherits, 3) formal param for method 4) case 5) dispatch *)

(* main must have no arguments *)

(* the rules for self *)
(* okay in dispatch: foo() -> self.foo() *)
(* The identifier self may be referenced, but it is an error to assign to self or to bind
self in a let, a case, or as a formal parameter. It is also illegal to have attributes named self .*)