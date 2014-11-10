open Core.Std;;

type node = 
| Prog of posnode list
| VarField of field
| Class of classrec
 and posnode = node * Lexing.position
 and field = { fieldname : string; fieldtype : string }
 and classrec = { classname : string; inherits : string; features : posnode list };;

type expr = 
  (* | `Not of posexpr *)
  | Assign of posexpr * posexpr
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
  | Sispatch of posexpr * posexpr
  | Dispatch of posexpr * posexpr (*this is very liberal, could we *)
  (* restrict the right side using types? *)
  | Id of idrec
  | Int of string
  | Str of string 
  | Bool of bool 


and posexpr = expr * Lexing.position	   
and idrec = {name:string; typ:string option};;
