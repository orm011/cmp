open Core.Std;;

type node = 
| Prog of posnode list
| VarField of field
| Class of classrec
 and posnode = node * Lexing.position
 and field = { fieldname : string; fieldtype : string }
 and classrec = { classname : string; inherits : string; features : posnode list };;
