open Core.Std;;

type lexpos = {
		fname : string;
   	lnum : int;
   	cnum : int;
		} with sexp

val convert : Lexing.position -> lexpos

module TypeId : sig
    type t with sexp (* cannot be self-type *)
    type tvar = Absolute of t | SelfType with sexp
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
    type id = Name of t | Self with sexp
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

type formal = ObjId.t * TypeId.t with sexp

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
	init : posexpr option;
} with sexp
		
type posfield = fieldr * lexpos with sexp
				
type methodr = { 
	methodname: MethodId.t; 
	formalparams: (formal * lexpos) list;  (* yes, formals with position are printed by reference *)
	returnType: TypeId.tvar; 
	defn:posexpr
	} with sexp
	
type posmethod = methodr * lexpos with sexp

(* used only for the parser *)
type feature = ParserMethod of methodr | ParserField of fieldr  

type cool_class = { 
	classname : TypeId.t; 
	inherits : TypeId.t; 
	methods : posmethod list; 
	fields : posfield list 
} with sexp
		
type posclass = cool_class * lexpos with sexp
type prog = posclass list with sexp
type posprog = prog * lexpos with sexp
 
exception ParseError of lexpos with sexp