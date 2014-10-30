open Core.Std;;
open Cool;;

let pad str = (String.make 2 ' ') ^ str;;

let rec to_lines posnode =
  let sub = let padded strlist = List.map ~f:pad strlist
	    in function
  | (Prog (clslist), _) ->
     ["_program"] @ padded (List.concat (List.map ~f:to_lines  clslist))
  | (Class ({classname;inherits;features}), pos) ->
     ["_class"; pad classname; pad inherits; pad "\"" ^ pos.Lexing.pos_fname ^ "\""] @
       padded (["("] @ (List.concat (List.map ~f:to_lines  features)) @ [")"])
  | (VarField ({fieldname; fieldtype}), _) ->
     ["_attr"; pad fieldname; pad fieldtype]
  in let (_, p) = posnode
     in ["#" ^ string_of_int p.Lexing.pos_lnum] @ sub posnode

(* let () = if (Array.length Sys.argv)  = 2 then 
(tokenize_file Sys.argv.(1)) else (tokenize_from_to stdin stdout);;*)
let () = 
     let infile = Sys.argv.(1) in 
     let inch = In_channel.create infile in
     let lexbuf = Lexing.from_channel inch in
     lexbuf.lex_curr_p <- { lexbuf.lex_start_p with pos_fname = infile};
     let prg = Cool_parse.program Cool_lexer.read lexbuf
     in Printf.printf "%s\n" (String.concat ~sep:"\n" (to_lines prg));;
