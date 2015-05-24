open Core.Std;;
open Cool;;
open Cool_tools;;
open Cool_lexer;;

let pad str = (String.make 2 ' ') ^ str;;

let padded strlist = List.map ~f:pad strlist

let rec lines_of_posexpr posexpr = match posexpr with
  | {expr; pos; exprtyp; } -> ["#" ^ string_of_int pos.lnum] @
                              (lines_of_expr expr) @ [ ": " ^ match exprtyp with 
    | None -> "_no_type" 
    | Some(typ) ->  TypeId.string_of_tvar typ
    ] 
and cat_expr a b = (lines_of_posexpr a) @ (lines_of_posexpr b)
and lines_of_field {  fieldname; fieldtype; init } = 
		[ObjId.string_of_t fieldname; TypeId.string_of_tvar fieldtype;] @ (lines_of_posexpr init)
and lines_of_branch {branchname; branchtype;  branche}
  = ["_branch"] @ padded([ObjId.string_of_t branchname; TypeId.string_of_t branchtype] @ (lines_of_posexpr branche))
and lines_of_expr (expr : Cool.expr) = match expr with 
  | ExprError -> failwith "why print expr error?"
  | Let {decls; letbody} -> ["_let"]  @ padded ((List.concat (List.map decls ~f:lines_of_field)) @ 
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
  | Assign(a,b) -> ["_assign"] @ padded ( [ObjId.string_of_t a]  @ (lines_of_posexpr b))
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
  | Id(i) -> [ "_object"; ] @ padded [ObjId.string_of_id i]
  | Int(str) ->  [ "_int"] @ padded [str]
  | Str(str) -> [ "_string" ] @ padded ["\"" ^ (Cool_lexer.print_escaped_string str) ^ "\""]
  | Bool(b) -> [ "_bool" ] @ padded [if b then "1" else "0" ]
  | NoExpr -> ["_no_expr"]
and lines_of_dispatch {obj; dispatchType; id; args } = match dispatchType with
  | None -> ["_dispatch"]  @ padded  ( ( lines_of_posexpr obj ) @ [ MethodId.string_of_t id; "("  ] @ 
                                       (List.concat ( List.map args ~f:lines_of_posexpr )) @ [ ")" ] )
  | Some(typ) -> ["_static_dispatch" ] @ padded ( (lines_of_posexpr obj) @ [ TypeId.string_of_t typ; MethodId.string_of_t  id; "("]  @
                                                  (List.concat ( List.map args ~f:lines_of_posexpr )) @ [ ")" ] )

						
let lines_of_ps (printer:'a -> string list)  ((node, pos):('a * lexpos)) =   
		["#" ^ (string_of_int pos.lnum)] @ (printer node)

let lines_of_attr f = 
		["_attr";] @ (padded (lines_of_field f))

let lines_of_formal (a,b) = 
		["_formal"] @ (padded  [ObjId.string_of_t a; TypeId.string_of_t b])
			
let lines_of_method { methodname; formalparams; returnType; defn } = 
    ["_method"] @ padded ([ MethodId.string_of_t methodname; ] @
                          (List.concat (List.map formalparams ~f:(fun form -> lines_of_ps lines_of_formal form)))
                          @ [TypeId.string_of_tvar returnType] @ lines_of_posexpr defn)


let lines_of_class pos {classname;inherits;methods; fields}  =
		["_class"; 
		pad (TypeId.string_of_t classname); 
		pad (TypeId.string_of_t inherits); 
		pad "\"" ^ pos.fname ^ "\""] @ 
		padded (["("] @ 
			let methodpos = List.map methods ~f:(fun (m,p) -> (lines_of_ps lines_of_method (m,p), p.lnum)) in
			let fieldpos  = List.map fields ~f:(fun (f,p) -> (lines_of_ps lines_of_attr (f,p), p.lnum)) in
			let sorted = List.sort ~cmp:(fun (_ ,p) -> fun (_,q) -> p - q) (methodpos @ fieldpos) in
			let only = List.map sorted ~f:(fun (str,_) -> str)
			in (List.concat only) @ [")"])

(* padded (fieldprint fieldrec) *)
	(* match posnode with                                                                            *)
  (* | (ParseError, _) -> failwith "why print parse error?"                                        *)
  (* | (VarField (fieldrec), _) ->                        *)
  (* | (Formal (a,b),_) ->  *)
  	(* and fieldprint {fieldname; fieldtype; init}                                                    *)
(* and lines_of_posnode posnode =                                                            *)
(*   let (_, p) = posnode in ["#" ^ string_of_int p.lnum] @ lines_of_node posnode *)


let lines_of_prog (clslist : prog) = 
		["_program"] @ padded 
			(List.concat (List.map ~f:(fun ((cls,pos) : posclass) -> lines_of_ps (lines_of_class pos) (cls,pos))  clslist))


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
  val superU: typegraph ->  TypeId.t -> TypeId.t ->  TypeId.t
	
	val parent: typegraph -> TypeId.t -> TypeId.t option
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
      | Some (n1) -> conforms gr n1 n2;;

  let get_path gr t : TypeId.t list = 
    let rec helper gr t acc = 
      match TypeId.Map.find gr t with
      | None -> acc
      | Some (p) -> helper gr p (p :: acc)
    in helper gr t [];;

  let revlcprefix l1 l2 = 
    let rec helper l1 l2 acc = 
      match l1 with
      | [] -> acc
      | f1 :: rest1 -> 
        (match l2 with 
         | [] -> acc
         | f2 :: rest2 -> if f1 = f2 then helper rest1 rest2 (f1 :: acc) else acc)
    in helper l1 l2 []

  (* what happens if the input type was never added to the graph? *)
  let superU (gr: typegraph) (t1:  TypeId.t) (t2: TypeId.t) : TypeId.t = 
    let p1 = get_path gr t1 in
    let p2 = get_path gr t2 in
    let rev = revlcprefix p1 p2
    in match rev with 
    | [] -> failwith "all prefixes have an obj, illegal input?"
    | h ::  _ -> h

  let defined (gr:typegraph) t = match Map.find gr t with 
    | None -> false
    | Some(_) -> true

	let parent (gr :typegraph) (t:TypeId.t) =  Map.find gr t			
end

let get_class_graph (prog : prog) 
  : (Conforms.typegraph, string) Result.t = 
  let add_class gr  { classname ; inherits ; _ }  = 
      Conforms.add_edge gr (classname, inherits) in 
  let add_opt (gr : (Conforms.partial_typegraph, string) Result.t) (c, _) = 
    (match gr with 
     | Ok (g) -> add_class g c
     | Error(_) as e -> e) in 
  let partial  = let init = Ok (Conforms.initial ()) 
		in List.fold_left prog ~init ~f:add_opt in 
  let almost = (match partial with 
      | Ok (g) -> g
      | Error(s) -> failwith s) in 
  Conforms.finish almost
	

module type ObjTableT = sig
    type t
    val add_obj: t -> ObjId.t * TypeId.tvar -> t (* id must be real but type  may be self type *)
    val get_obj: t -> ObjId.t -> TypeId.tvar option
    val empty: t
end

module ObjTable : ObjTableT = struct 
    type t = TypeId.tvar ObjId.Map.t
    let add_obj t (id, typ) = 
      ObjId.Map.add t ~key:id ~data:typ
    let get_obj t id = 
      ObjId.Map.find t id
    let empty = ObjId.Map.empty
end

module type MethodTableT = sig
    type t
    type methsig = { params: TypeId.t list; ret: TypeId.tvar }

    val empty: t
    val add_meth: t -> TypeId.t -> methodr -> t
    val get_meth: t -> TypeId.t * MethodId.t -> methsig option
end
			     
module MethodTable : MethodTableT = struct
    module FullId = 
      Comparable.Make
	(
	  struct 
	    type t = TypeId.t * MethodId.t with sexp, compare
	  end
	)

    type methsig = { params: TypeId.t list; ret: TypeId.tvar }
				   
    type t = methsig FullId.Map.t
		     
    let methsig_of_formal {formalparams; returnType; _} = 
      let nf = List.map formalparams ~f:(fun ((_, typ), _) -> typ)
      in  { params=nf; ret=returnType }

    let empty = FullId.Map.empty
 
    let add_meth m t ({ methodname; _ } as mrec) = 
      FullId.Map.add m ~key:(t, methodname) ~data:(methsig_of_formal mrec)

    let get_meth m pr = FullId.Map.find m pr 
end

let get_method_table (prog:prog) : MethodTable.t = 
	let absorb_class (t : MethodTable.t) (({classname; methods; _}, _) : posclass) : MethodTable.t = 
			List.fold_left ~init:t ~f:(fun t -> fun (m,_) -> (MethodTable.add_meth t classname m)) methods  
				in List.fold_left ~init:MethodTable.empty ~f:absorb_class prog

(* gets the table mapping all class fields to their types *)
(* this table has all the symbols defined locally in the class *) 
let get_class_object_table ({fields; _}: cool_class) : ObjTable.t = 
    let pairs = List.map fields ~f:(fun ({fieldname; fieldtype; _}, _) -> (fieldname, fieldtype)) in
    List.fold pairs ~init:ObjTable.empty ~f:ObjTable.add_obj  	
																
let get_fields_table (prog:prog) : ObjTable.t TypeId.Map.t =
	let absorb_class (t : ObjTable.t TypeId.Map.t) (({classname;  _} as cls, _)  : posclass)	=
		TypeId.Map.add t ~key:classname ~data:(get_class_object_table cls)
		in List.fold_left ~init:TypeId.Map.empty ~f:absorb_class prog

type global_context = {	
			(* global context. does not change *)
			(* invariants: g and fields and methods tables all have data for the same types *)
			fields			:	ObjTable.t TypeId.Map.t; 
			methods			:	MethodTable.t; 
			g						:	Conforms.typegraph 
	}

let get_global_context (prog:prog) : global_context = {
	fields=get_fields_table prog; 
	methods=get_method_table prog; 
	g=(match get_class_graph prog with 
		| Ok(g) -> g 
		| _ -> failwith "graph not done") } 		
		
type expression_context = {
			local				:	ObjTable.t; 
			dynamic_cls	:	TypeId.t; 
			lexical_cls	:	TypeId.t; 			
			global			: global_context;
	}

let rec field_lookup (global: global_context) (starting:TypeId.t)  (name:ObjId.t) : TypeId.tvar option = 
	match TypeId.Map.find global.fields starting with 
	| None -> failwith "type map must have all types"
	| Some(tab) -> (match ObjTable.get_obj tab name with 
		| Some(x) -> Some(x) (* found it ! *) 	
		| None -> (match Conforms.parent global.g starting with
			| Some(parent) -> field_lookup global parent name  (* lookup in the parent *)
			| None -> None (* reached end of parent chain, defined nowhere *) 
	))
	
exception SemanticError of Cool.posexpr with sexp;;
	
let name_lookup (context:expression_context) (name:ObjId.id) : TypeId.tvar option  = 
  match name with 
  | ObjId.Name n -> 
			(match ObjTable.get_obj context.local n with
				| (Some(_) as x) -> x  (* defined within method *)
				| None -> field_lookup context.global context.lexical_cls n ) 
  | ObjId.Self -> Some(TypeId.SelfType)

let rec typecheck_posexpr (context : expression_context) ({expr; _} as posex : posexpr) : posexpr option = 
  match typecheck_expr context expr with 
  | Some(e, t) -> Some { posex with expr=e; exprtyp=Some(t) }
  | None -> failwith "typecheck failed"
and typecheck_expr (context : expression_context) (e:expr)  : (expr * TypeId.tvar) option =
	let inttype = TypeId.Absolute(TypeId.intt) in 
	let stringtype = TypeId.Absolute(TypeId.stringt) in 
	let booltype = TypeId.Absolute(TypeId.boolt) in
  match e with
  | Int(_) -> Some(e, inttype)
	| Str(_) -> Some(e, stringtype)
	| Bool(_) -> Some(e, booltype)
	| New(tt) -> Some(e, tt) (* TODO: need to check the type actually exists. maybe in a previous pass *)
  | Id(name)  -> (match (name_lookup context name)  with 
		      | None -> failwith "not found" 
		      | Some (t) -> Some ( e, t ) )
  | Plus(l, r) -> (
    match Option.both (typecheck_posexpr context l) (typecheck_posexpr context r) with
       | Some (lt, rt) -> if (Option.both l.exprtyp r.exprtyp) = Some (inttype, inttype)
	  											then Some (Plus(lt, rt), inttype) 
													else None
       | None -> None)
  | _ -> failwith "expression not implemented"


let typecheck_method (classname : TypeId.t) ( global :  global_context ) (methodr : methodr) : methodr option = 
	let {formalparams; defn; returnType; _} = methodr in 
	let f = fun (tab: ObjTable.t) -> fun ((name, t), _) -> ObjTable.add_obj tab (name, Absolute(t)) in
	let local =  List.fold_left formalparams ~init:ObjTable.empty ~f in
	let checked_impl = typecheck_posexpr { local;  dynamic_cls=classname; lexical_cls=classname; global } defn in
	match checked_impl with 
	| None -> None (* failed type checking *)
	| Some(x) -> (let ret = Some {methodr with defn=x} in 
		match (x.exprtyp, returnType) with 
		| (None,_) -> failwith "should have a type now. this is a compiler bug"
		| (Some(TypeId.SelfType), TypeId.SelfType) -> ret
		| (Some(TypeId.Absolute(actual)), TypeId.Absolute(expected)) -> 
				if Conforms.conforms global.g actual expected then ret else None
		| _ -> None)

let typecheck_class (global : global_context) cool_class : cool_class option = 
	let {methods; classname; _} = cool_class in
	let checked = List.map ~f:(fun (m,_) -> typecheck_method classname global m) methods in
	match Option.all checked with 
	| None -> None
	| Some(l) -> let checkedmethods = List.map (List.zip_exn l methods) ~f:(fun (mm, (_,pos)) -> (mm, pos)) 
		in Some {cool_class with methods=checkedmethods}
		
let typecheck_prog prog : prog option = 
	let global = get_global_context prog in
		let checked = List.map ~f:(fun (cls,_) -> typecheck_class global cls) prog in
		match Option.all checked with 
		| None -> None
		| Some(l) -> let checkedclasses = List.map (List.zip_exn l prog) ~f:(fun (cls, (_,pos)) -> (cls, pos)) 
		in Some checkedclasses


(* 
Notes on semantic checks:

context needed
   O(v)
   M(C, f) = (t0,...,tn, ret)
   C to resolve self type. 
   typegraph to determine conformance

semantic rule list:
0) self type: only on method return, let, and field decl.
1) self : only as a reference. never as a  declaration. never in an assignment.
1) method table is globally visible
2) fields are only visible inside.
3) attribute initialization order
4) SELF_TYPE in checking.

TODO:
 add all builtin info to global context
 check that all type ids in the program exist in the tables (?)
 deal with non-existing names (?)
 fields type checking (inited properly? ordering?)
 method lookup (dispatch)
 SELF_TYPE rules (when does it make a difference? still a bit confused)
 error propagation (eg, type error, or name not found)
*)

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
      | Some(p) ->   let (prgnopos, pos) = p in 
					(match typecheck_prog prgnopos with 
					| None -> failwith "failed type check"
					| Some(completedp) -> lines_of_ps lines_of_prog (completedp, pos))
      | None -> (Cool_tools.syntax_error
                   (convert lexbuf.lex_start_p) lexbuf.lex_start_pos "top" ); ["Compilation halted due to lex and parse errors"] in
  Printf.printf "%s\n%!" (String.concat ~sep:"\n" (print_prg prg))

let () = parse_main ();;
