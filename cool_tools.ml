open Core.Std;;
open Cool;;

let errcount = ref 0
let debug = ref false


let err_count () = !errcount;;

let set_debug () = debug := true;;

let rec deflatten { decls; expr } = 
  let (_, pos) = expr in 
  match decls with
  | [] -> failwith "empty let declaration list" 
  | _ :: []  as singledecl -> {decls=singledecl; expr} 
  | hd :: tl -> { decls=[hd]; expr=(Let(deflatten { decls=tl; expr
						     }), pos) }

let debug_print str =
  if !debug then Printf.eprintf "(Debug) printing from %s\n%!" str else ()

let syntax_error startp startofs loc = 
  errcount := !errcount + 1;
  debug_print loc;
  Printf.eprintf "\"%s\", line %d: parse error at or near %d(%d)\n%!"  
		 startp.Lexing.pos_fname startp.Lexing.pos_lnum
		 (startofs - startp.Lexing.pos_bol) startofs;
