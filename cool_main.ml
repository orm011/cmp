open Core.Std;;
open Cool;;
open Cool_tools;;
open Cool_lexer;;


let pad str = (String.make 2 ' ') ^ str;;

let rec padded strlist = List.map ~f:pad strlist
and lines_of_posnode posnode = 
  let (_, p) = posnode in ["#" ^ string_of_int p.Lexing.pos_lnum] @ lines_of_node posnode

and lines_of_node posnode = match posnode with
  | (ParseError, _) -> failwith "why print parse error?"
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
  | {expr; pos; exprtyp; } -> ["#" ^ string_of_int pos.Lexing.pos_lnum] @
		   (lines_of_expr expr) @ [ ": " ^ match exprtyp with 
					    | None -> "_no_type" 
					    | Some(typ)
					      ->  match typ with 
						  | SelfType -> "SELF_TYPE"
						  | TypeId(x) -> x
 ] 
and cat_expr a b = (lines_of_posexpr a) @ (lines_of_posexpr b)

and fieldprint {fieldname; fieldtype; init} = [fieldname; fieldtype;] @ (lines_of_posexpr init)
and lines_of_branch {branchname; branchtype;  branche}
  = ["_branch"] @ padded([branchname; branchtype] @ (lines_of_posexpr branche))
and lines_of_expr (expr : Cool.expr) = match expr with 
  | ExprError -> failwith "why print expr error?"
  | Let {decls; letbody} -> ["_let"]  @ padded ((List.concat (List.map decls ~f:fieldprint)) @ 
(lines_of_posexpr letbody))
  | Block a -> ["_block"] @ padded (List.concat (List.map a ~f:lines_of_posexpr))
  | If {pred; thenexp; elseexp} -> ["_cond"] @ 
				     padded (List.concat 
					       (List.map 
						  [pred; thenexp; elseexp] 
						  ~f:lines_of_posexpr))
  | New a -> ["_new"] @ padded [a]
  | Loop { cond; body } -> ["_loop" ] @ padded ((lines_of_posexpr cond) @ (lines_of_posexpr body))
  | Case { test; branches} -> ["_typcase"] @ 
				padded (
				    (lines_of_posexpr test) 
				    @ (List.concat  
					 (List.map  
					    branches  
					    ~f:lines_of_branch)))
  | Assign(a,b) -> ["_assign"] @ padded ( [a.name]  @ (lines_of_posexpr b))
  | Comp(a) -> ["_comp" ] @ padded (lines_of_posexpr a)
  | Lequal(a,b) -> [ "_lte" ] @ padded (cat_expr a b)
  | Eq(a,b) -> [ "_eq" ] @ padded (cat_expr a b)
  | Lt(a,b) -> [ "_lt" ] @ padded (cat_expr a b)
  | Div(a,b) -> [ "_divide" ] @ padded (cat_expr a b)
  | Mult(a,b) -> [ "_mul" ] @ padded (cat_expr a b)
  | Minus(a,b) -> [ "_sub" ] @ padded (cat_expr a b)
  | Plus(a,b) -> [ "_plus" ] @ padded (cat_expr a b)
  | Dispatch(a) -> lines_of_dispatch a
  | Neg(a) -> ["_neg"] @ padded (lines_of_posexpr a)
  | IsVoid(a) -> ["_isvoid"] @ padded (lines_of_posexpr a)
  | Id(i) -> [ "_object"; ] @ padded [i.name]
  | Int(str) ->  [ "_int"] @ padded [str]
  | Str(str) -> [ "_string" ] @ padded ["\"" ^ (Cool_lexer.print_escaped_string str) ^ "\""]
  | Bool(b) -> [ "_bool" ] @ padded [if b then "1" else "0" ]
  | NoExpr -> ["_no_expr"]
and lines_of_dispatch {obj; dispatchType; id; args } = match dispatchType with
  | None -> ["_dispatch"]  @ padded  ( ( lines_of_posexpr obj ) @ [ id; "("  ] @ 
		(List.concat ( List.map args ~f:lines_of_posexpr )) @ [ ")" ] )
  | Some(typ) -> ["_static_dispatch" ] @ padded ( (lines_of_posexpr obj) @ [ typ; id; "("]  @
		(List.concat ( List.map args ~f:lines_of_posexpr )) @ [ ")" ] )


let tokenize_main () =
     for i = 1 to (Array.length Sys.argv - 1) do
     let infile = Sys.argv.(i) in
     let inch = In_channel.create infile in
     Printf.fprintf stdout "#name \"%s\"\n" infile;
     tokenize_from_to inch stdout;
     In_channel.close inch
     done

let parse_main () = 
     let infile = Sys.argv.(1) in 
     let inch = In_channel.create infile in
     let lexbuf = Lexing.from_channel inch in
     lexbuf.lex_curr_p <- { lexbuf.lex_start_p with pos_fname = infile };
     let prg = try Some(Cool_parse.program Cool_lexer.read lexbuf)
	       with _ -> None in 
     let print_prg prg =  
       if Cool_tools.err_count () > 0 
       then ["Compilation halted due to lex and parse errors"] 
       else match prg  with
	    | Some(p) -> lines_of_posnode p
	    | None -> (Cool_tools.syntax_error
			 lexbuf.lex_start_p lexbuf.lex_start_pos "top" ); ["Compilation halted due to lex and parse errors"] in
     Printf.printf "%s\n%!" (String.concat ~sep:"\n" (print_prg prg))


let () = parse_main ();;


(*
in a + b. need to be able to lookup a's type, + signature and return *)
(*value.

b.foo(a), need to look up b's type. t(b)
from the type, see if it or a parent has a method foo (the first),
then see  what the params are. get their type then get a's type t(a) < t

so, need to answer:
t(b) 
has_method(t(b)) (by type) get_method params.
t(a)
t(a) conforms to type x.
then the return type is the ret type of the method.
if the method has self type, then the reutnr method is the 
type of b.

b@a.foo(c)
need to look up b's type
check b < a.
check if a or an ancestor have a method.
so on. 


1+ b.
need to look up b's type.
check it is int.


if a then b else c. 
need to compute type for a, see it is boolean.
b need to compute type b
c need to compute type c.
then assign a type of 


O(v)
M(f) = (t0,...,tn)
C for self type.
conforms(t1, t2)
 *)
