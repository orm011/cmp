open Core.Std;;


module TypeId = struct
  module T = struct
      type t = string with sexp, compare (*invariant, never equal to SELF_TYPE *)
  end
  include T

  type tvar  = Absolute of t | SelfType with sexp, compare
  let tvar_of_string st = if st = "SELF_TYPE"  then SelfType else Absolute st
  let string_of_tvar = function 
    | SelfType -> "SELF_TYPE"
    | Absolute(t) -> t

  let t_of_tvar = function
    | SelfType -> failwith "selftype"
    | Absolute(t) -> t

let string_of_t t = t 
  let obj = tvar_of_string("Object")
  let objt = "Object"
  let intt = "Int"
  let stringt = "String"
  let boolt  = "Bool"
  include Comparable.Make(T)
end

module ObjId = struct
    module T = struct
	type t = string with sexp, compare
    end
    include T
    include Comparable.Make(T)
    type id = Name of t | Self
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

(* module MethodId = struct *)
(*     module T = struct *)
(* 	type t = string with sexp, compare *)
(*       end *)
(*     include T *)
(*     include Comparable.Make(T) *)
(*     let mid_of_string st = st *)
(*     let string_of_mid m = m *)
(*   end *)


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
	pos:Lexing.position;
	exprtyp:TypeId.tvar option 
	}
and fieldr = { 
	fieldname : ObjId.t; (* cannot be self *) 
	fieldtype : TypeId.tvar; (* can be self_type *) 
	init : posexpr;
}
		
type posfield = fieldr * Lexing.position
				
type methodr = { 
	methodname: MethodId.t; 
	formalparams: (formal * Lexing.position) list; 
	returnType: TypeId.tvar; 
	defn:posexpr
	}
	
type posmethod = methodr * Lexing.position

(* used only for the parser *)
type feature = ParserMethod of methodr | ParserField of fieldr  

type cool_class = { 
	classname : TypeId.t; 
	inherits : TypeId.t; 
	methods : posmethod list; 
	fields : posfield list 
}
		
type posclass = cool_class * Lexing.position

type prog = posclass list
type posprog = prog * Lexing.position
 

exception ParseError of Lexing.position
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