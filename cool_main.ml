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
  | (VarField (fieldrec), _) -> ["_attr";] @ padded (fieldprint fieldrec)
  | (Formal (a,b),_) -> ["_formal"] @ (padded  [a; b])
  | (Method { methodname; formalparams; returnType; defn }, _) ->
     ["_method"] @ padded ([ methodname; ] @ 
			     (List.concat (List.map formalparams ~f:
					  lines_of_posnode))
			     @ [returnType] @ lines_of_posexpr defn)
and lines_of_posexpr posexpr = match posexpr with
  | (expr, p) -> ["#" ^ string_of_int p.Lexing.pos_lnum] @
		   (lines_of_expr expr) @ [": _no_type"]
and cat_expr a b = (lines_of_posexpr a) @ (lines_of_posexpr b)

and fieldprint {fieldname; fieldtype; init} = 
  [fieldname; fieldtype]  @  (match init with
				     | None -> ["_no_expr"; ": _no_type"] 
				     | Some(x) -> lines_of_posexpr x )

and lines_of_expr (expr : Cool.expr) = match expr with 
  | Let {decls; expr} -> ["_let"]  @ padded ((List.concat (List.map decls ~f:fieldprint)) @ 
(lines_of_posexpr expr))
  | Block a -> ["_block"] @ padded (List.concat (List.map a ~f:lines_of_posexpr))
  | If {pred; thenexp; elseexp} -> ["_cond"] @ padded (List.concat (List.map [pred; thenexp; elseexp] ~f:lines_of_posexpr))
  | Assign(a,b) -> ["_assign"] @ padded ( [a.name]  @ (lines_of_posexpr b))
  | Comp(a) -> ["_comp" ] @ padded (lines_of_posexpr a)
  | Lequal(a,b) -> [ "_lte" ] @ padded (cat_expr a b)
  | Equal(a,b) -> [ "_eq" ] @ padded (cat_expr a b)
  | Less(a,b) -> [ "_lt" ] @ padded (cat_expr a b)
  | Div(a,b) -> [ "_divide" ] @ padded (cat_expr a b)
  | Mult(a,b) -> [ "_mul" ] @ padded (cat_expr a b)
  | Minus(a,b) -> [ "_sub" ] @ padded (cat_expr a b)
  | Plus(a,b) -> [ "_plus" ] @ padded (cat_expr a b)
  | Dispatch(a) -> lines_of_dispatch a
  | Neg(a) -> ["_neg"] @ padded (lines_of_posexpr a)
  | IsVoid(a) -> ["_isvoid"] @ padded (lines_of_posexpr a)
  | Id(i) -> [ "_object"; ] @ padded [i.name]
  | Int(str) ->  [ "_int"] @ padded [str]
  | Str(str) -> [ "_string" ] @ padded ["\"" ^ str ^ "\""]
  | Bool(b) -> [ "_bool" ] @ padded [if b then "1" else "0" ]
and lines_of_dispatch {obj; dispatchType; id; args } = match dispatchType with
  | None -> ["_dispatch"]  @ padded  ( ( lines_of_posexpr obj ) @ [ id; "("  ] @ 
		(List.concat ( List.map args ~f:lines_of_posexpr )) @ [ ")" ] )
  | Some(typ) -> ["_static_dispatch" ] @ padded ( (lines_of_posexpr obj) @ [ typ; id; "("]  @
		(List.concat ( List.map args ~f:lines_of_posexpr )) @ [ ")" ] )

let () = 
     let infile = Sys.argv.(1) in 
     let inch = In_channel.create infile in
     let lexbuf = Lexing.from_channel inch in
     lexbuf.lex_curr_p <- { lexbuf.lex_start_p with pos_fname = infile
			  };
     let prg = Cool_parse.program Cool_lexer.read lexbuf
     in Printf.printf "%s\n" (String.concat ~sep:"\n" (lines_of_posnode prg))
