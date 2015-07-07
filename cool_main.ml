open Core.Std;;
open Cool;;
open Cool_tools;;
open Cool_lexer;;
open Coolout;;


module type ObjTableT = sig
  type t
  val add_obj: t -> ObjId.t * TypeId.tvar -> t (* id must be real but type  may be self type *)
  val get_obj: t -> ObjId.t -> TypeId.tvar option
  val empty: t
	val to_alist: t -> (ObjId.t *TypeId.tvar) list
	val of_alist: (ObjId.t *TypeId.tvar) list -> t
end

module ObjTable : ObjTableT = struct 
  type t = TypeId.tvar ObjId.Map.t
  let add_obj t (id, typ) = 
    ObjId.Map.add t ~key:id ~data:typ
  let get_obj t id = 
    ObjId.Map.find t id
  let empty = ObjId.Map.empty
	let to_alist m  = ObjId.Map.to_alist m
	let of_alist l = ObjId.Map.of_alist_exn l
end

module type MethodTableT = sig
  type t
  val empty: t
  val add_meth: t -> methodr -> t
	val add_sig: t  ->  MethodId.t -> MethodId.methsig -> t
  val get_meth: t -> MethodId.t -> MethodId.methsig option
	val to_alist: t -> (MethodId.t * MethodId.methsig) list
	val of_alist: (MethodId.t * MethodId.methsig) list -> t
end

module MethodTable : MethodTableT = struct
	open MethodId;;

  type t = methsig MethodId.Map.t

  let methsig_of_formal {formalparams; returnType; _} = 
    let nf = List.map formalparams ~f:(fun ((_, typ), _) -> typ)
    in  { params=nf; ret=returnType }

  let empty =  MethodId.Map.empty (*TypeId.objt {methodname="abort"; }*)

  let add_meth m ({ methodname; _ } as mrec) = 
    MethodId.Map.add m ~key:methodname ~data:(methsig_of_formal mrec)

	let add_sig m methodname methsig =  MethodId.Map.add m ~key:methodname ~data:methsig

	let get_meth m n = MethodId.Map.find m n 
	
	let to_alist m  = Map.to_alist m
	let of_alist m = Map.of_alist_exn m
end



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
	val children_exn: typegraph -> TypeId.t -> TypeId.t list
	val sexp_of_typegraph : typegraph -> Sexp.t
end

module Conforms : ConformsType = struct
  type partial_typegraph = TypeId.t TypeId.Map.t
  type typegraph = {parent:TypeId.t TypeId.Map.t; children:(TypeId.t list) TypeId.Map.t}

  (* only used here because we know the *)
  (* literal input strings we are using *)
  let t_of_string s = match TypeId.tvar_of_string s with
    | TypeId.SelfType -> failwith "unexpected selftype"
    | TypeId.Absolute (t) -> t

  let objT = TypeId.Builtin.obj
  let ioT =  TypeId.Builtin.io

  (* cannot inherit from these below *)
  let strT = TypeId.Builtin.string
  let intT = TypeId.Builtin.int
  let boolT = TypeId.Builtin.bool
	

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

 	(* checks the class graph is well formed:*)
	(* 1. every class has a defined parent, except object. *)
  let finish (gr : partial_typegraph) : (typegraph, string) Result.t  = 
    if (List.for_all (TypeId.Map.keys gr) ~f:(fun x -> rooted gr x))
		then let merge_fun ~(key:TypeId.t) ~(data:TypeId.t) (mp:TypeId.t list TypeId.Map.t)  =
			let oldentry = Option.value ~default:[] (Map.find mp data) in
			let newentry = key :: oldentry in 
			let rev = Map.add mp ~key:data ~data:newentry in 
			match Map.find rev key with 
			| None -> Map.add rev ~key:key ~data:[] (* add empty list for leaves *)
			| Some t -> rev
		in  let children = Map.fold gr ~init:TypeId.Map.empty ~f:merge_fun in 
    Ok {parent=gr; children} else Error "some class is not well grounded"

  let rec conforms (gr : typegraph) n1 n2  = 
    if n1 = n2 then true else
      match Map.find gr.parent n1 with
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
    let p1 = get_path gr.parent t1 in
    let p2 = get_path gr.parent t2 in
    let rev = revlcprefix p1 p2
    in match rev with 
    | [] -> failwith "all prefixes have an obj, illegal input?"
    | h ::  _ -> h

  let defined gr t = rooted gr.parent t 
  (* every defined type in a complete typegraph is rooted, so they are equivalent. deals with base case already *)

  let parent (gr :typegraph) (t:TypeId.t) =  Map.find gr.parent t
	
	let children_exn (gr :typegraph) (t:TypeId.t) = Map.find_exn gr.children t
	
	let sexp_of_typegraph (gr:typegraph ):  Sexp.t  = TypeId.Map.sexp_of_t (List.sexp_of_t TypeId.sexp_of_t)  gr.children

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

let get_class_tables ({fields; methods; _}: cool_class) : (ObjTable.t* MethodTable.t) = 
  let pairs = List.map fields ~f:(fun ({fieldname; fieldtype; _}, _) -> (fieldname, fieldtype)) in
  let objt = List.fold pairs ~init:ObjTable.empty ~f:ObjTable.add_obj in 
	let methodt = List.fold methods ~init:MethodTable.empty ~f:(fun mt (m, _) -> MethodTable.add_meth mt m) in (objt, methodt)
	
let get_tables (prog:prog) (init:(ObjTable.t TypeId.Map.t * MethodTable.t TypeId.Map.t)): (ObjTable.t TypeId.Map.t * MethodTable.t TypeId.Map.t) =
  let absorb_class ((f,m) : (ObjTable.t TypeId.Map.t * MethodTable.t TypeId.Map.t))  (({classname;  _} as cls, _) : posclass) =
		let (fd, md) = get_class_tables cls in 
    let newf = TypeId.Map.add f ~key:classname ~data:fd in 
		let newd = TypeId.Map.add m ~key:classname ~data:md in (newf, newd)
  in List.fold_left ~init ~f:absorb_class prog

type global_context = {
  (* global context. does not change *)
  (* invariants: g and fields and methods tables all have data for the same types *)
  fields   : ObjTable.t TypeId.Map.t; 
  methods  : MethodTable.t TypeId.Map.t; 
  g      : Conforms.typegraph 
}

let get_global_context (prog:prog) : global_context =   
	let g = (match get_class_graph prog with 
      	| Ok(g) -> g 
      	| _ -> failwith "graph not done") in
	let builtin_fields = List.fold Cool.builtin_fields ~init:TypeId.Map.empty ~f:(fun map (typ, lst) -> TypeId.Map.add map ~key:typ ~data:(ObjTable.of_alist lst)) in
	let builtin_methods = List.fold Cool.builtin_methods ~init:TypeId.Map.empty ~f:(fun map (typ, lst) -> TypeId.Map.add map ~key:typ ~data:(MethodTable.of_alist lst))in
	let (fields, methods) = get_tables prog (builtin_fields,builtin_methods) in  {fields; methods; g} 

type expression_context = {
  local    : ObjTable.t; 
  cls : TypeId.t; 
  global   : global_context;
}

let rec method_lookup (ctx: global_context) (classid:TypeId.t) (mid:MethodId.t) : (TypeId.t * MethodId.methsig) option = 
	let open MethodId in
	let open TypeId in 
	match TypeId.Map.find ctx.methods classid with 
	| None -> failwith "undefined type"
	| Some tab -> match MethodTable.get_meth tab mid with 
		| (Some sigt) as ret -> Some (classid, sigt)
		| None -> if classid = Builtin.obj then None else
			let parent_type = Option.value_exn (Conforms.parent ctx.g classid) in 
			method_lookup ctx parent_type mid 

let rec field_lookup (global: global_context) (starting:TypeId.t)  (name:ObjId.t) : TypeId.tvar option = 
  match TypeId.Map.find global.fields starting with 
  | None -> failwith "undefined type"
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

let inttype = TypeId.Absolute TypeId.Builtin.int;;
let stringtype = TypeId.Absolute TypeId.Builtin.string;; 
let booltype = TypeId.Absolute TypeId.Builtin.bool;;
let objtype = TypeId.Absolute TypeId.Builtin.obj;;

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
			| TypeId.SelfType -> Option.value_exn checked.obj.exprtyp
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
	let open MethodId in 
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
  let ctx = { local=ObjTable.empty; cls=classname; global } in
  match init with 
  | None -> Some(fieldr) (* no type problems if no init *)
  | Some initsome ->  let checked_init = expr_conforms_exn ctx initsome fieldtype 
		in Some { fieldr with init=Some checked_init }

(* methods must be checked with local field definitions available *)
(* what about field definitions? are they defined with local method definitions available?. what about other fields?*)
(* (how will codegen work here, in terms of what gets inited first? *)
(* methods that override existing methods must take more general inputs, or return more specific outputs *)
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
notes re. SelfType: 

okay in:
1) method return type 
2) field declaration type 
3) let declaration type  
4) argument to new

not okay in:
1) class name
2) inherits
3) formal param for method 
4) case expression
5) dispatch expression

Within every class C's scope, there is a name self with type SelfType(C).

notes re. restrictions:
1) main must have no arguments 
2) self may be referenced. 
3) (re. self) it is an error to:
	a) assign to self 
	b) bind self in a let, a case, or as a formal parameter. 
	c) have attributes named self .

Field rules:
1) fields are only visible inside their (transitive) classes.
2) fields are visible in other field inits.
3) fields are visible from methods.

Check there is a class for all typeId used . done.
Field names override. done
shadowing not allowed. done.

No redefinition of fields. 
No redefinition of methods. 
No redefinition of self (in fields or let or args)

Dispatch:
   0. typecheck of obj expression.
	 1. typecheck of all argument expressions
   2. figure out the dispatchType to use (given or, self type or type of obj)
   3. verify the type of the obj expression is compatible with the dispatchType
   4. lookup the method signature given the dispatch type and name. verify it exists
   5. for each of the param types, check expressions are compatible with param type.
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

let check_method_conforms (g : global_context ) methsig1 methsig2 = methsig1 = methsig2 (* later on can loosen up *)

(* assumes every class except object is rooted on object.*)
(* checks all method overrides are safe. *)
(* checks there are no field redefinitions. *)
let check_inheritance_rules (g: global_context) : unit = 
	let rec helper (g:global_context) (occ:ObjTable.t) (mcc:MethodTable.t) (current:TypeId.t) : unit = 
		let methods = TypeId.Map.find_exn g.methods current in 
		let fields = TypeId.Map.find_exn g.fields current in
		let folding_methods mcc (id, methsig) = let _ = (match MethodTable.get_meth mcc id with | None -> () 
		| Some s -> if check_method_conforms g methsig s then () else failwith "method override has incompatible signature") 
			in MethodTable.add_sig mcc id methsig in
		let folding_fields  occ (id, typ) = let _ = (match ObjTable.get_obj occ id with None -> () 
		| Some t -> failwith "field redefined") 
			in ObjTable.add_obj occ (id, typ) in
		let newmcc = List.fold (MethodTable.to_alist methods) ~init:mcc ~f:folding_methods in
		let newocc = List.fold (ObjTable.to_alist fields) ~init:occ ~f:folding_fields in
		ignore(List.map (Conforms.children_exn g.g current) ~f:(fun x -> helper g newocc newmcc x)) in
		helper g ObjTable.empty MethodTable.empty TypeId.Builtin.obj

(* runs all semantic checks on prog *)
(*let _ = Printf.printf "%s\n%!" (Sexp.to_string_hum (Conforms.sexp_of_typegraph global.g)) in *)
let typecheck_prog prog : prog option = 
  let global = get_global_context prog in
  let _ = check_abs_types global.g prog in
	let _ = check_inheritance_rules global in 
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
  let infile = if (Array.length Sys.argv <=1) then failwith "Usage: filename <verbose=true/false>" else Sys.argv.(1) in
	let verbose = if (Array.length Sys.argv) <= 2 then false else (bool_of_string Sys.argv.(2)) in  
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
				| Seman { msg;  expr; bt;} -> (Printf.printf "%s: %s.\nCompilation halted due to static semantic errors.\n%!" (string_of_pos expr.pos) msg); if verbose 
				then Printf.printf "%s\n%!" bt else ();  exit 1) with 
         | None -> failwith "failed type check"
         | Some(completedp) -> lines_of_ps lines_of_prog (completedp, pos))
      | None -> (Cool_tools.syntax_error
                   (convert lexbuf.lex_start_p) lexbuf.lex_start_pos "top" ); ["Compilation halted due to lex and parse errors"] in
  Printf.printf "%s\n%!" (String.concat ~sep:"\n" (print_prg prg))

let () = parse_main ();;