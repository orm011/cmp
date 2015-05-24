open Core.Std;;
open Cool;;

let errcount = ref 0
let debug = ref false

let err_count () = !errcount;;

let set_debug () = debug := true;;

let untyped_expr (expr:expr) (pos:lexpos) = 
		   { expr; pos; exprtyp=None; }


let rec deflatten { decls; letbody } = 
  let { pos; _ } = letbody in 
  match decls with
  | [] -> failwith "empty let declaration list" 
  | _ :: []  as singledecl -> {decls=singledecl; letbody} 
  | hd :: tl -> { decls=[hd]; letbody=(untyped_expr (Let (deflatten { decls=tl; letbody
						     }))  pos) }

let debug_print str =
  if !debug then Printf.eprintf "(Debug) printing from %s\n%!" str else ()

let syntax_error (startp : lexpos) startofs loc = 
  errcount := !errcount + 1;
  debug_print loc;
  Printf.eprintf "\"%s\", line %d: parse error at or near %d\n%!"  
		 startp.fname startp.lnum
		 (startofs - startp.bol);;

