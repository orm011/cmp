open Core.Std;;
open Cool;;
open Cool_tools;;
open Cool_lexer;;
open Coolout;;

(*
Well formed type definition:
  -tree structure rooted at object.
  -no descendents to Int, String or Bool.  
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

  let defined = rooted 
  (* every defined type in a complete typegraph is rooted, so they are equivalent. deals with base case already *)

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
  let absorb_class (t : ObjTable.t TypeId.Map.t) (({classname;  _} as cls, _)  : posclass) =
    TypeId.Map.add t ~key:classname ~data:(get_class_object_table cls)
  in List.fold_left ~init:TypeId.Map.empty ~f:absorb_class prog

type global_context = {
  (* global context. does not change *)
  (* invariants: g and fields and methods tables all have data for the same types *)
  fields   : ObjTable.t TypeId.Map.t; 
  methods   : MethodTable.t; 
  g      : Conforms.typegraph 
}

let get_global_context (prog:prog) : global_context = {
  fields=get_fields_table prog; 
  methods=get_method_table prog; 
  g=(match get_class_graph prog with 
      | Ok(g) -> g 
      | _ -> failwith "graph not done") } 

type expression_context = {
  local    : ObjTable.t; 
  cls : TypeId.t; 
  global   : global_context;
}

let rec method_lookup (ctx: global_context) (classid:TypeId.t) (mid:MethodId.t) : (TypeId.t * MethodTable.methsig) option = 
	match MethodTable.get_meth ctx.methods (classid, mid) with 
	| (Some sigt) as ret -> Some (classid, sigt)
	| None -> if classid = TypeId.objt then None else
			let parent_type = Option.value_exn (Conforms.parent ctx.g classid) in 
			method_lookup ctx parent_type mid 

let rec field_lookup (global: global_context) (starting:TypeId.t)  (name:ObjId.t) : TypeId.tvar option = 
  match TypeId.Map.find global.fields starting with 
  | None -> failwith "type map must have all types"
  | Some(tab) -> (match ObjTable.get_obj tab name with 
      | Some(x) -> Some(x) (* found it ! *) 
      | None -> (match Conforms.parent global.g starting with
          | Some(parent) -> field_lookup global parent name  (* lookup in the parent *)
          | None -> None (* reached end of parent chain, defined nowhere *) 
        ))

let name_lookup (context:expression_context) (name:ObjId.id) : TypeId.tvar option  = 
  match name with 
  | ObjId.Name n -> 
    (match ObjTable.get_obj context.local n with
     | (Some(_) as x) -> x  (* defined within method *)
     | None -> field_lookup context.global context.cls n ) 
  | ObjId.Self -> Some(TypeId.SelfType)

(* uses: field decl, let decl *)
let tvar_ok (gr : Conforms.typegraph) (tv : TypeId.tvar) : bool = 
  let open TypeId in match tv with 
  | SelfType -> true
  | Absolute t -> Conforms.defined gr t;;

let type_compatible (cls: TypeId.t) (g: Conforms.typegraph) (actual : TypeId.tvar) (expected : TypeId.tvar) : bool =
  let open TypeId in match (actual, expected) with 
  | SelfType, SelfType -> true
  | SelfType, Absolute expectedt  -> Conforms.conforms g cls expectedt
  (* if SELF_TYPE_classname <: expected, then for all C <: classname, it is also true *)        
  | Absolute _, SelfType -> false (* would need actualt <: SELF_TYPE_C for all C <: classname  *)
  | Absolute actualt, Absolute expectedt -> Conforms.conforms g actualt expectedt ;;

let resolve_bound ctx tvar : TypeId.t = 
	let open TypeId in match tvar with 
		|	Absolute t -> t
		| SelfType -> ctx.cls;;

let join ctx tvar1 tvar2 : TypeId.tvar =
	let (t1, t2) = (resolve_bound ctx tvar1, resolve_bound ctx tvar2) in
	Absolute (Conforms.superU ctx.global.g t1 t2);;

let inttype = TypeId.Absolute(TypeId.intt);;
let stringtype = TypeId.Absolute(TypeId.stringt);; 
let booltype = (TypeId.Absolute TypeId.boolt);;
let objtype = TypeId.obj;;

type semanrec = { msg:string; bt:string; expr:posexpr } with sexp;;
exception Seman of semanrec with sexp;;

let rec typecheck_posexpr (context : expression_context) ({expr; _} as posex : posexpr) : posexpr = 
  let  (echecked, etype) = (try typecheck_expr context expr with 
	| Failure msg -> raise (Seman {msg=msg; bt=Exn.backtrace(); expr=posex}) )  in { posex with expr=echecked; exprtyp=Some etype }  
and typecheck_expr (context : expression_context) (e:expr) : (expr * TypeId.tvar) =
  let g = context.global.g in 
  match e with
  | Int _ -> e, inttype
  | Str _ -> e, stringtype
  | Bool _ -> e, booltype
  | New tt -> e, (match tt with 
    | TypeId.SelfType -> tt
    | TypeId.Absolute t -> if Conforms.defined g t then tt else (failwith "type not found"))
  | Id name  -> (match name_lookup context name  with 
      | None -> failwith "name not found" 
      | Some (t) -> e, t )
  | Intop (b, l, r) -> 
    let chl = (typecheck_posexpr context l) in 
    let chr =  (typecheck_posexpr context r) in 
    (Intop (b, chl, chr), if (Option.both chl.exprtyp chr.exprtyp) = Some (inttype, inttype) then inttype else failwith "binop: both must be int")
	| Intcomp  (b, l, r) ->
		let chl = (typecheck_posexpr context l) in
		let chr = (typecheck_posexpr context r) in 
		let x = (Option.both chl.exprtyp chr.exprtyp) in (match x with 
			| Some (tvar1, tvar2) -> (Intcomp (b, chl, chr), if tvar1 = inttype && tvar2 = inttype then booltype else failwith "bincomp: both must be int")
			| None -> failwith "unexpected")
	| Comp e -> let che = typecheck_posexpr context e in 
		(Comp che, if che.exprtyp = Some inttype then inttype else failwith "complement: must be int")
	| Neg e ->  let che = typecheck_posexpr context e in 
		(Neg che, if che.exprtyp = Some booltype then booltype else failwith "neg: must be boolean") 
	| IsVoid e -> let che = typecheck_posexpr context e in 
	 (IsVoid che, match che.exprtyp with 
		| Some t -> booltype
		| None -> failwith "unexpected")
	| Assign (var, e) ->  let typeid  = (match name_lookup context (ObjId.Name var) with 
		| None -> failwith "assign: name undefined"
		| Some t -> t) in let checkede = expr_conforms_exn context e typeid in (Assign (var, checkede), Option.value_exn checkede.exprtyp)
	| Dispatch ({obj; dispatchType; id; args} as drec) -> let (checked, ret) = typecheck_dispatch context drec in 
		let rettype = (match ret with 
			| TypeId.SelfType -> Option.value_exn obj.exprtyp
			|(TypeId.Absolute _) as abs -> abs) in (Dispatch checked, rettype)
	| Eq (l, r) -> 
		let chl = (typecheck_posexpr context l) in
		let chr = (typecheck_posexpr context r) in 
		let x = (Option.both chl.exprtyp chr.exprtyp) in (match x with 
			| Some (tvar1, tvar2) -> (Eq (chl, chr), if eq_ok tvar1 tvar2 then booltype else failwith "eq not okay")
			| None -> failwith "unexpected")
	| Let r -> let r = (typecheck_let context r) in  let t =  (match r.letbody.exprtyp with 
					| None -> failwith "unexpected"
					| Some t -> t) in (Let r, t)
	| Block exprs -> let mapped = List.map ~f:(fun e -> typecheck_posexpr context e) exprs in
		let { exprtyp; _ } = List.last_exn mapped in (match exprtyp with 
		| Some t -> Block mapped, t
		| None -> failwith "unexpected")
	| If { pred; thenexp; elseexp } -> let checked = typecheck_if context pred thenexp elseexp in 
		let (t1, t2) = Option.value_exn (Option.both checked.thenexp.exprtyp checked.elseexp.exprtyp) in 
		let newtyp = join context t1 t2 
		in (If checked, newtyp)
	| Loop {cond; body} -> let checked_cond = expr_conforms_exn context cond booltype in 
		let checked_body = expr_conforms_exn context body objtype in (Loop {cond=checked_cond; body=checked_body},  objtype)
  | _ -> failwith (Printf.sprintf "expression not implemented: %s" (Sexp.to_string_hum (sexp_of_expr e)))
and eq_ok tvar1 tvar2 : bool  = 
	let isspecial = fun t -> (let special = [booltype; stringtype; inttype] in ((List.find special ~f:(fun x -> x = t)) <> None)) in
	if (isspecial tvar1) || (isspecial tvar2) then tvar1 = tvar2 else true
and typecheck_dispatch ctx ({obj; dispatchType; id; args} as drec) : (dispatchrec * TypeId.tvar) =  
(* 
   0. typecheck of obj expression.
	 1. typecheck of all argument expressions
   2. figure out the dispatchType to use (given or, self type or type of obj)
   3. verify the type of the obj expression is compatible with the dispatchType
   4. lookup the method signature given the dispatch type and name. verify it exists
   5. for each of the param types, check expressions are compatible with param type.
*)
	let checked_obj = typecheck_posexpr ctx obj in
	let objt = Option.value_exn checked_obj.exprtyp in
	let checked_args = List.map args ~f:(fun a -> typecheck_posexpr ctx a) in  
	let lookup_tid =  (match dispatchType with 
	| None -> resolve_bound ctx objt (* it could be self_type *)
	| Some t -> t) in
	if not (type_compatible ctx.cls ctx.global.g objt (TypeId.Absolute lookup_tid)) then  
		failwith "object type does not conform to dispatch type" else
	let open MethodTable in 
	let (actual_cls, {params; ret}) = (match method_lookup ctx.global lookup_tid id  with 
		| None -> failwith "method id not found"
		| Some signature -> signature) in 
	let paired = List.zip_exn checked_args params in 
	let _ = List.map paired ~f:(fun (posexp, t) -> expr_conforms_exn ctx posexp (TypeId.Absolute t)) in
	({drec with obj=checked_obj; args=checked_args; (*dispatchType=Some (Absolute actual_cls)*)}, ret)
and typecheck_let ctx {decls; letbody} : letrec = 
	match decls with 
	| [] -> {decls=[]; letbody=(typecheck_posexpr ctx letbody)}
	| ({	fieldname; fieldtype; init } as initrec) :: rest ->
		let checked_init = (match init with
			|  None -> None  
			|  Some initexp ->  Some (expr_conforms_exn ctx initexp fieldtype)) in
		let innerctx = {ctx with local=(ObjTable.add_obj ctx.local (fieldname, fieldtype)) } in
		let {decls=checked_rest; letbody=checked_body} = (typecheck_let innerctx {decls=rest; letbody}) in
		  { decls={ initrec with init=checked_init } :: checked_rest; letbody=checked_body } 	
and typecheck_if ctx pred thenexp elseexp = (* first check pred *)
	let checkedpred = expr_conforms_exn ctx pred booltype in 
	let checkedthen = typecheck_posexpr ctx thenexp in
	let checkedelse = typecheck_posexpr ctx elseexp in
	{pred=checkedpred; thenexp=checkedthen; elseexp=checkedelse }
and expr_conforms_exn (ctx: expression_context) (exp:posexpr) (expected:TypeId.tvar) : posexpr = 
	let checked_exp = typecheck_posexpr ctx exp in
			match checked_exp.exprtyp with 
		| None -> failwith "expr not well typed: "
		| Some t -> if type_compatible ctx.cls ctx.global.g t expected then checked_exp else failwith "expr not conformant to expected"


let typecheck_method (classname : TypeId.t) ( global :  global_context ) (methodr : methodr) : methodr option = 
  let {formalparams; defn; returnType; _} = methodr in 
  let f = fun (tab: ObjTable.t) -> fun ((name, t), _) -> ObjTable.add_obj tab (name, Absolute(t)) in
  let local =  List.fold_left formalparams ~init:ObjTable.empty ~f in
	let ctx = { local;  cls=classname; global } in
  let checked_defn = expr_conforms_exn ctx defn returnType in Some {methodr with defn=checked_defn}

let typecheck_field (classname : TypeId.t) (global : global_context) fieldr : fieldr option = 
  let {fieldtype; init; _} = fieldr in 
  let ctx = {local=ObjTable.empty; cls=classname; global } in
  match init with 
  | None -> Some(fieldr) (* no type problems if no init *)
  | Some initsome ->  let checked_init = expr_conforms_exn ctx initsome fieldtype 
		in Some { fieldr with init=Some checked_init }

let typecheck_class (global : global_context) cool_class : cool_class option = 
  let {methods; fields; classname; _} = cool_class in
  let checkedm = Option.all (List.map ~f:(fun (m ,_) -> typecheck_method classname global m) methods) in
  let checkedf = Option.all (List.map ~f:(fun (fi, _) -> typecheck_field classname global fi) fields) in 
  match (checkedm, checkedf) with 
  | Some ms, Some fs -> 
    let checkedmethods = List.map (List.zip_exn ms methods) ~f:(fun (mm, (_,pos)) -> (mm, pos)) in
    let checkedfields =  List.map (List.zip_exn fs fields) ~f:(fun (ff, (_,pos)) -> (ff, pos))
    in Some {cool_class with methods=checkedmethods; fields=checkedfields}
  | _ -> None

(*
the formal params is a list of formals with position
but to print them we need them to be posnodes


(* the nature of Self_Type: *)
(* okay in: 1) method return type 2) field declaration type 3) let declaration type  4) argument to new *)
(* What is the meaning of Self_Type in those cases above *)
not okay in 1) class name, 2) inherits, 3) formal param for method 4) case 5) dispatch

main must have no arguments 
the rules for self 
okay in dispatch: foo() -> self.foo()
The identifier self may be referenced, but it is an error to assign to self or to bind
self in a let, a case, or as a formal parameter. It is also illegal to have attributes named self .

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

 TypeIds: check they exist at:
    -new 
    -field declaration type
    -method return type
    -method param declaration type
    -let expr
    -case
    (use s exp tree instead)
   
/Field names: ancestor field names accessible?  
      A: in ref, yes. but, if name reused with a different type, then seems to complain
      A: in manual: no explanation?
      For now, we will do shadowing. so the innermost definition wins.
      
 Method override  type checks.
 Add all builtin basic class info to global context
 //Check that all type ids in the program exist in the tables (?)
 //Deal with non-existing names (?) 
 //Done. fields type checking (inited properly? ordering?)
*)

(* check all type ids used in the program are well defined elsewhere *)
let check_abs_types typegraph prog : bool =
  let lookup t = Conforms.defined typegraph (TypeId.t_of_sexp t) in 
  let open Sexp in let rec check_abs_types_helper (sprog : Sexp.t) : bool =
    (match sprog with 
    | (List [Atom("Tid");  _]) as t -> if lookup t then true else failwith (Printf.sprintf "unknown type %s" (Sexp.to_string t )) 
    | Atom _ -> true
    | List l -> List.fold_left ~init:true ~f:(&&) (List.map ~f:check_abs_types_helper l))
  in check_abs_types_helper (Cool.sexp_of_prog prog)

(* runs all semantic checks on prog *)
let typecheck_prog prog : prog option = 
  let global = get_global_context prog in
  let _ = check_abs_types global.g prog in
  let checked = List.map ~f:(fun (cls,_) -> typecheck_class global cls) prog in
  match Option.all checked with 
  | None -> None
  | Some(l) -> let checkedclasses = List.map (List.zip_exn l prog) ~f:(fun (cls, (_,pos)) -> (cls, pos)) 
    in Some checkedclasses

let tokenize_main () =
  for i = 1 to (Array.length Sys.argv - 1) do
    let infile = Sys.argv.(i) in
    let inch = In_channel.create infile in
    Printf.fprintf stdout "#name \"%s\"\n" infile;
    tokenize_from_to inch stdout;
    In_channel.close inch
  done

let string_of_pos ps = 
	ps.fname ^ ":" ^ (string_of_int ps.lnum);;

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
        (match (try typecheck_prog prgnopos with 
				| Seman { msg;  expr; _} -> (Printf.printf "%s: %s.\nCompilation halted due to static semantic errors.\n%!" (string_of_pos expr.pos) msg); exit 1) with 
         | None -> failwith "failed type check"
         | Some(completedp) -> lines_of_ps lines_of_prog (completedp, pos))
      | None -> (Cool_tools.syntax_error
                   (convert lexbuf.lex_start_p) lexbuf.lex_start_pos "top" ); ["Compilation halted due to lex and parse errors"] in
  Printf.printf "%s\n%!" (String.concat ~sep:"\n" (print_prg prg))

let () = parse_main ();;