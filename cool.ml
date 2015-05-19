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
    type id = Name of t | Self | Dummy
    let t_of_id = function
      | Name (t) -> t
      | Self -> failwith "id was not absolute"
      | Dummy -> failwith "wth. Dummy"
    let id_of_string st = if st = "self" then Self else Name st
    let string_of_id v = match v with
      | Self -> "self"
      | Name(t) -> t
      | Dummy -> failwith "dont print tree with errors"
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


(* TODO remove this typename alias *)
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
(* TODO: idtyp: should it be ObjId.id or ObjId.t?, or should it be 
there at all? *)
and idrec = {name:ObjId.id;  idtyp:typename option}
(* TODO: these shouldn't be variants *)
and node =
| Prog of posnode list
| VarField of field
| Class of classrec
| Method of methodrec
| Formal of ObjId.id * typename
| ParseError
 and posnode = node * Lexing.position
 and field = { fieldname : ObjId.id; fieldtype : typename;
	       init : posexpr }
(* TODO: classname should be a TypeId.t *)
 and classrec = { classname : typename; inherits : typename;
		  features : posnode list }
 and methodrec = { methodname: MethodId.t; formalparams: posnode list;
		 returnType: typename; defn:posexpr};;
 
