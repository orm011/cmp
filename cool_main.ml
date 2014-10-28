open Core.Std;;
open Cool;;

let pad str = (String.make 2 ' ') ^ str;;

let rec to_lines = 
    let padded strlist = List.map ~f:pad strlist in function
  | Prog (clslist) -> 
     ["_program"] @ padded (List.concat (List.map ~f:to_lines  clslist))
  | Class ({classname;inherits;filename;features}) ->  
     ["_class"; pad classname; pad inherits; pad "\"" ^ filename ^ "\""] @
       padded (["("] @ (List.concat (List.map ~f:to_lines  features)) @ [")"])
  | VarField ({fieldname; fieldtype}) -> 
     ["_attr"; pad fieldname; pad fieldtype]


(* let () = if (Array.length Sys.argv)  = 2 then 
(tokenize_file Sys.argv.(1)) else (tokenize_from_to stdin stdout);;*)
let () = 
     let infile = Sys.argv.(1) in 
     let inch = In_channel.create infile in
     let prg = Cool_parse.program Cool_lexer.read (Lexing.from_channel
						     inch) 
     in Printf.printf "%s\n" (String.concat ~sep:"\n" (to_lines prg));;
