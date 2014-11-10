open Core.Std;;
open Cool;;

let pad str = (String.make 2 ' ') ^ str;;

let rec padded strlist = List.map ~f:pad strlist
and lines_of_posnode posnode = 
  let (_, p) = posnode in ["#" ^ string_of_int p.Lexing.pos_lnum] @ lines_of_node posnode
and lines_of_node posnode = match posnode with
  | (Prog (clslist), _) ->
     ["_program"] @ padded (List.concat (List.map ~f:lines_of_posnode  clslist))
  | (Class ({classname;inherits;features}), pos) ->
     ["_class"; pad classname; pad inherits; pad "\"" ^ pos.Lexing.pos_fname ^ "\""] @
       padded (["("] @ (List.concat (List.map ~f:lines_of_posnode  features)) @ [")"])
  | (VarField ({fieldname; fieldtype}), _) ->
     ["_attr"; pad fieldname; pad fieldtype]
and lines_of_posexpr posexpr = match posexpr with
  | (expr, p) -> ["#" ^ string_of_int p.Lexing.pos_lnum] @ (lines_of_expr expr)
and cat_expr a b = (lines_of_posexpr a) @ (lines_of_posexpr b)
and lines_of_expr (expr : Cool.expr) = match expr with 
 (* | `Not(pe) -> *)
  | Assign(a,b) -> ["_assign"] @ padded (cat_expr a b)
  | Comp(a) -> ["_comp" ] @ padded (lines_of_posexpr a)
  | Lequal(a,b) -> [ "_lte" ] @ padded (cat_expr a b) (*need to verify *)
				(*string _lte *)
  | Equal(a,b) -> [ "_eq" ] @ padded (cat_expr a b)
  | Less(a,b) -> [ "_lt" ] @ padded (cat_expr a b)
  | Div(a,b) -> [ "_div" ] @ padded (cat_expr a b)
  | Mult(a,b) -> [ "_mult" ] @ padded (cat_expr a b)
  | Minus(a,b) -> [ "_sub" ] @ padded (cat_expr a b)
  | Plus(a,b) -> [ "_plus" ] @ padded (cat_expr a b)
  | Dispatch(a,b) -> [ "_dispatch" ] @ padded (cat_expr a b) 
  | Sispatch(a,b) -> ["_static_dispatch"] @ padded (cat_expr a b)
  | Neg(a) -> ["_neg"] @ padded (lines_of_posexpr a)
  | IsVoid(a) -> ["_isvoid"] @ padded (lines_of_posexpr a)
  | Id(i) -> [ "_object"; i.name]
  | Int(str) ->  [ "_int"] @ padded [str]
  | Str(str) -> [ "_string" ] @ padded [str]
  | Bool(b) -> [ "_bool" ] @ padded [string_of_bool b]


(* let () = if (Array.length Sys.argv)  = 2 then 
(tokenize_file Sys.argv.(1)) else (tokenize_from_to stdin stdout);;*)
let () = 
     let infile = Sys.argv.(1) in 
     let inch = In_channel.create infile in
     let lexbuf = Lexing.from_channel inch in
     lexbuf.lex_curr_p <- { lexbuf.lex_start_p with pos_fname = infile};
     let prg = Cool_parse.exprtop Cool_lexer.read lexbuf
     in List.iter prg ~f:fun prg -> Printf.printf "%s\n" (String.concat ~sep:"\n" (lines_of_posexpr prg));;
