open Core.Std;;

type lexpos = {
		fname : string;
   	lnum : int;
   	cnum : int;
		} with sexp (* defining our own position type so that it has sexp representation *)

let convert ({pos_fname; pos_lnum; pos_cnum; pos_bol} : Lexing.position) : lexpos =
			{ fname=pos_fname; lnum = pos_lnum; cnum = pos_cnum  - pos_bol;}

module TypeId = struct
  module T = struct
     	type t = Tid of string with sexp, compare (*invariant, never equal to SELF_TYPE *)
  end
  include T

  type tvar  = Absolute of t | SelfType with sexp, compare
  let tvar_of_string st = if st = "SELF_TYPE"  then SelfType else Absolute (Tid st)
  let string_of_tvar = function 
    | SelfType -> "SELF_TYPE"
    | Absolute(Tid(t)) -> t

  let t_of_tvar = function
    | SelfType -> failwith "selftype"
    | Absolute(t) -> t

  let string_of_t (Tid t) = t 
  let obj = tvar_of_string("Object")
  let objt = Tid "Object"
  let intt = Tid "Int"
  let stringt = Tid "String"
  let boolt  = Tid "Bool"
  include Comparable.Make(T)
end
	
module ObjId = struct
    module T = struct
      type t = string with sexp, compare
    end
    include T
    include Comparable.Make(T)
    type id = Name of t | Self with sexp
    let t_of_id = function
      | Name (t) -> t
      | Self -> failwith "id was not absolute"
    let id_of_string st = if st = "self" then Self else Name st
    let string_of_id v = match v with
      | Self -> "self"
      | Name(t) -> t
		let string_of_t t = t
end

module MethodId = struct
    module T = struct 
	type t = string with sexp, compare
      end
    include T
    include Comparable.Make(T)
    let t_of_objid = function 
      | ObjId.Name(t) -> t
      | _ -> failwith "method should not have name self or dummy"
    let string_of_t t = t 
  end

type formal = ObjId.t * TypeId.t with sexp

type binop = Plus | Minus | Mult | Div with sexp
type bincomp = Lequal | Lt | Eq with sexp

type expr =
  | Let of letrec
	| Intcomp of bincomp * posexpr * posexpr
	| Intop of binop * posexpr * posexpr
	| Eq of posexpr * posexpr
  | Comp of posexpr
  | Neg of posexpr
  | IsVoid of posexpr
  | Assign of ObjId.t * posexpr
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
	exprtyp:TypeId.tvar option 
	}
and fieldr = { 
	fieldname : ObjId.t; (* cannot be self *) 
	fieldtype : TypeId.tvar; (* can be self_type *) 
	init : posexpr option;
} with sexp
		
type posfield = fieldr * lexpos with sexp
				
type methodr = { 
	methodname: MethodId.t; 
	formalparams: (formal * lexpos) list; 
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
 
exception ParseError of lexpos