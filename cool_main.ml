open Core.Std;;
open Cool;;
open Cool_tools;;
open Cool_lexer;;
open Lexing;;

let pad str = (String.make 2 ' ') ^ str;;

let rec padded strlist = List.map ~f:pad strlist
and lines_of_posnode posnode = 
  let (_, p) = posnode in ["#" ^ string_of_int p.Lexing.pos_lnum] @ lines_of_node posnode

and lines_of_node posnode = match posnode with
  | (ParseError, _) -> failwith "why print parse error?"
  | (Prog (clslist), _) ->
     ["_program"] @ padded (List.concat (List.map ~f:lines_of_posnode  clslist))
  | (Class ({classname;inherits;features}), pos) ->
     ["_class"; pad (TypeId.string_of_tvar classname); pad (TypeId.string_of_tvar inherits); pad "\"" ^ pos.Lexing.pos_fname ^ "\""] @
       padded (["("] @ (List.concat (List.map ~f:lines_of_posnode  features)) @ [")"])
  | (VarField (fieldrec), _) -> ["_attr";] @ padded (fieldprint fieldrec)
  | (Formal (a,b),_) -> ["_formal"] @ (padded  [a; TypeId.string_of_tvar b])
  | (Method { methodname; formalparams; returnType; defn }, _) ->
     ["_method"] @ padded ([ methodname; ] @ 
			     (List.concat (List.map formalparams ~f:
					  lines_of_posnode))
			     @ [TypeId.string_of_tvar returnType] @ lines_of_posexpr defn)
and lines_of_posexpr posexpr = match posexpr with
  | {expr; pos; exprtyp; } -> ["#" ^ string_of_int pos.Lexing.pos_lnum] @
		   (lines_of_expr expr) @ [ ": " ^ match exprtyp with 
					    | None -> "_no_type" 
					    | Some(typ) ->  TypeId.string_of_tvar typ
 ] 
and cat_expr a b = (lines_of_posexpr a) @ (lines_of_posexpr b)

and fieldprint {fieldname; fieldtype; init}
  = [fieldname; TypeId.string_of_tvar fieldtype;] @ (lines_of_posexpr init)
and lines_of_branch {branchname; branchtype;  branche}
  = ["_branch"] @ padded([branchname; TypeId.string_of_tvar branchtype] @ (lines_of_posexpr branche))
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
  | New a -> ["_new"] @ padded [ TypeId.string_of_tvar a ]
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
  | Some(typ) -> ["_static_dispatch" ] @ padded ( (lines_of_posexpr obj) @ [ TypeId.string_of_tvar typ; id; "("]  @
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
     lexbuf.Lexing.lex_curr_p <- { lexbuf.lex_start_p with pos_fname = infile };
     let prg = try Some(Cool_parse.program Cool_lexer.read lexbuf)
	       with _ -> None in 
     let print_prg prg  : string list =  
       if Cool_tools.err_count () > 0
       then ["Compilation halted due to lex and parse errors"] 
       else match prg  with
	    | Some(p) -> lines_of_posnode p
	    | None -> (Cool_tools.syntax_error
			 lexbuf.lex_start_p lexbuf.lex_start_pos "top" ); ["Compilation halted due to lex and parse errors"] in
     Printf.printf "%s\n%!" (String.concat ~sep:"\n" (print_prg prg))

let () = parse_main ();;


(*
well formed type definition:
-every type conforms to object
-

different cases:
-chain is incomplete because some type definition hasn't been read yet
 *)
module type ConformsType = sig
    type partial_typegraph (* typegraph being constructed *)
    type typegraph (* fully validated type graph *)
    val initial: unit -> partial_typegraph
    val add_edge: partial_typegraph -> TypeId.t * TypeId.t ->
		  (partial_typegraph,  string) Result.t
    (* will complain if adding edge violates properties*)
		    
    val finish: partial_typegraph -> (typegraph , string) Result.t
    (* will complain if current typegraph violates properties*)

    (* queries *)
    val defined: typegraph -> TypeId.t -> bool
    val conforms: typegraph -> TypeId.t -> TypeId.t -> bool

end

module Conforms : ConformsType = struct
    type partial_typegraph = TypeId.t TypeId.Map.t
    type typegraph = TypeId.t TypeId.Map.t

    (* only used here because we know the *)
    (* literal input strings we are using *)
    let t_of_string s = match TypeId.tvar_of_string s with
      | TypeId.SelfType -> failwith "unexpected selftype"
      | TypeId.Absolute (t) -> t

    let objT = t_of_string "Object"
    let ioT =  t_of_string "IO"

    (* cannot inherit from these below *)
    let strT = t_of_string "String"
    let intT = t_of_string "Int"
    let boolT = t_of_string "Bool"

    let initial () = TypeId.Map.of_alist_exn [ (strT, objT); (intT, objT); 
				(ioT, objT); (boolT, objT) ]

    let rec has_cycle_with_edge (gr:TypeId.t TypeId.Map.t) (a,b) = 
      if a = b then true else 
	match TypeId.Map.find gr b with
	| None -> false
	| Some(next) -> has_cycle_with_edge gr (a, next)

    (* need to check:
1) would it inherit from some of the special classes
2) would it redefine a type with the same name added before (names are global)
3) would it create a class cycle
     *)
    let add_edge (gr:partial_typegraph) (n1, n2) =
      match Map.find gr n1 with
      | Some(_) -> Error "cannot redefine type"
      | None -> if n2 = strT || n2 = intT || n2 = boolT then Error "cannot extend str/int/bool"
      		else if has_cycle_with_edge gr (n1, n2) then Error "would add cycle"
		else Ok (TypeId.Map.add gr ~key:n1 ~data:n2)

    (* assumes no cycles in graph. checks if type has full
    inheritance chain defined up to object *)
    let rec rooted gr t = 
      if t = objT then true 
      else match TypeId.Map.find gr t with
	   | None -> false
	   | Some(p) -> rooted gr p

    let finish (gr : partial_typegraph) : (typegraph, string) Result.t  = 
      if (List.for_all (TypeId.Map.keys gr) ~f:(fun x -> rooted gr x))
	   then Ok gr else Error "some class is not well grounded"
      
    let rec conforms (gr : typegraph) n1 n2  = 
      if n1 = n2 then true else
	   match Map.find gr n1 with
	   | None -> false
	   | Some (n1) -> conforms gr n1 n2

    let defined (gr:typegraph) t = match Map.find gr t with 
      | None -> false
      | Some(_) -> true

end

(* todo: make sure no SELF_TYPE is used in classes *)
let get_class_graph (prog : node) 
    : (Conforms.typegraph, string) Result.t = 
  let add_class gr node = match node with  
    | Class (c) -> let { classname ; inherits ; _ } = c in
		   let actualclass = (
		     match classname with 
		     | TypeId.SelfType -> failwith "cannot name class
						    self type"
		     | TypeId.Absolute(t) -> t
		   ) in 
		   let actualinh = (
		     match inherits with
		     | TypeId.SelfType -> failwith "cannot inherit from self type"
		     | TypeId.Absolute(t) -> t 
		   ) in 
		   Conforms.add_edge gr (actualclass, actualinh)
    | _ -> failwith "not a class" in 
  let add_opt (gr : (Conforms.partial_typegraph, string) Result.t) (node, _) = 
    (match gr with 
    | Ok (g) -> add_class g node
    | Error(_) as e -> e) in 
  let partial =( match prog with 
     | Prog ( classes ) -> 
	let init = Ok (Conforms.initial ()) in
	List.fold_left classes ~init ~f:add_opt
     | _ -> failwith "not a prog") in 
  let almost = (match partial with 
     | Ok (g) -> g
     | Error(s) -> failwith s) in 
  Conforms.finish almost


   
  


(* maps needed:
O(v)
M(f) = (t0,...,tn)
C for self type.
conforms(t1, t2)
 *)
