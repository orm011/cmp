open Core.Std;;

type expr = 
  | Let of letrec
  | Assign of idrec * posexpr
  | Comp of posexpr
  | Lequal of posexpr * posexpr
  | Less of posexpr * posexpr
  | Equal of posexpr * posexpr 
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
  | New of string
  | Loop of looprec
  | Case of caserec
  | NoExpr
  | ExprError
and caserec = { test:posexpr; branches:branch list}
and branch  = { branchname:string; branchtype:string;  branche:posexpr }
and looprec =  { cond:posexpr; body:posexpr }
and ifrec = { pred:posexpr; thenexp:posexpr; elseexp:posexpr }
and letrec = { decls: field list; expr: posexpr }
and dispatchrec = {obj:posexpr; dispatchType:string option; id:string; args:posexpr list}
and posexpr = expr * Lexing.position
and idrec = {name:string; typ:string option}
and node = 
| Prog of posnode list
| VarField of field
| Class of classrec
| Method of methodrec
| Formal of string * string
| ParseError
 and posnode = node * Lexing.position
 and field = { fieldname : string; fieldtype : string; init : posexpr }
 and classrec = { classname : string; inherits : string;
		  features : posnode list }
 and methodrec = { methodname: string; formalparams: posnode list; 
		 returnType: string; defn:posexpr}
(* the formal params is a list of formals with position
but to print them we need them to be posnodes *)
