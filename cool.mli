open Core.Std;;

type node = 
| Prog of node list   
| VarField of varFieldRec
| Class of classRec
 and varFieldRec = { fieldname : string; fieldtype : string }
 and classRec = { classname : string; inherits : string;
		  filename : string; features : node list };;

