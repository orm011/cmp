open Core.Std;;

(* let () = if (Array.length Sys.argv)  = 2 then 
(tokenize_file Sys.argv.(1)) else (tokenize_from_to stdin stdout);;*)
let () = 
     let infile = Sys.argv.(1) in 
     let inch = In_channel.create infile in
     let prg = Cool_parse.program Cool_lexer.read (Lexing.from_channel
						     inch) in ignore(prg);;
