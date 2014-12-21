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
  let obj = tvar_of_string("Object")
 
  include Comparable.Make(T)
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
and branch  = { branchname:string; branchtype:typename;  branche:posexpr }
and looprec =  { cond:posexpr; body:posexpr }
and ifrec = { pred:posexpr; thenexp:posexpr; elseexp:posexpr }
and letrec = { decls: field list; letbody: posexpr }
and dispatchrec = {obj:posexpr; dispatchType:typename option; id:string; args:posexpr list}
and posexpr = { expr:expr;
		pos:Lexing.position;
		exprtyp:typename option }
and idrec = {name:string; idtyp:typename option}
and node =
| Prog of posnode list
| VarField of field
| Class of classrec
| Method of methodrec
| Formal of string * typename
| ParseError
 and posnode = node * Lexing.position
 and field = { fieldname : string; fieldtype : typename; init : posexpr }
 and classrec = { classname : typename; inherits : typename;
		  features : posnode list }
 and methodrec = { methodname: string; formalparams: posnode list;
		 returnType: typename; defn:posexpr};;
 
