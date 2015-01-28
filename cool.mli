open Core.Std;;

module TypeId : sig
    type t with sexp
    type tvar = Absolute of t | SelfType
    val tvar_of_string: string -> tvar
    val string_of_tvar: tvar -> string
    val t_of_tvar: tvar -> t
    include Comparable.S with type t := t
    val obj : tvar
    val objt : t
    val intt : t
    val stringt : t
    val boolt : t
end

module ObjId : sig
    type t with sexp
    include Comparable.S with type t := t
    type id = Name of t | Self | Dummy 
    val id_of_string: string -> id
    val string_of_id: id -> string
end

module MethodId : sig
    type t with sexp
    include Comparable.S with type t:= t
    val t_of_objid: ObjId.id -> t
    val string_of_t: t -> string
end

type typename = TypeId.tvar

type expr =
  | Let of letrec
  | Assign of idrec * posexpr
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
  | Id of idrec
  | Int of string
  | Str of string
  | Bool of bool
  | Block of posexpr list
  | If of ifrec
  | New of typename
  | Loop of looprec
  | Case of caserec
  | NoExpr
  | ExprError
and caserec = { test:posexpr; branches:branch list}
and branch  = { branchname:ObjId.id; branchtype:typename;  branche:posexpr }
and looprec =  { cond:posexpr; body:posexpr }
and ifrec = { pred:posexpr; thenexp:posexpr; elseexp:posexpr }
and letrec = { decls: field list; letbody: posexpr }
and dispatchrec = {obj:posexpr; dispatchType:typename option; id:MethodId.t; args:posexpr list}
and posexpr = { expr:expr;
		pos:Lexing.position;
		exprtyp:typename option }
and idrec = {name:ObjId.id; idtyp:typename option}
and node =
| Prog of posnode list
| VarField of field
| Class of classrec
| Method of methodrec
| Formal of ObjId.id * typename
| ParseError
 and posnode = node * Lexing.position
 and field = { fieldname : ObjId.id; fieldtype : typename; init : posexpr }
 and classrec = { classname : typename; inherits : typename;
		  features : posnode list }
 and methodrec = { methodname: MethodId.t; formalparams: posnode list;
		 returnType: typename; defn:posexpr};;
 
(* the formal params is a list of formals with position
but to print them we need them to be posnodes *)
