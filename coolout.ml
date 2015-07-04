open Core.Std;;
open Cool;;
open Cool_tools;;
open Cool_lexer;;

(* these output tools match the format for the reference impl *)
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
  [ObjId.string_of_t fieldname; TypeId.string_of_tvar fieldtype;] @ (
     match init with 
    | None ->  ["_no_expr"; ": _no_type"] 
    | Some(posex) -> lines_of_posexpr posex)
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

let show_posexpr (e : posexpr) = 
	Printf.printf "%s\n%!" (Sexp.to_string_hum (sexp_of_posexpr e))

let show_expr (e : expr) = 
	Printf.printf "%s\n%!" (Sexp.to_string_hum (sexp_of_expr e))

let lines_of_prog (clslist : prog) = 
  ["_program"] @ padded 
    (List.concat (List.map ~f:(fun ((cls,pos) : posclass) -> lines_of_ps (lines_of_class pos) (cls,pos))  clslist))

(* prints ast following ref impl format *)
let dump_ast_ref posprog =
  Printf.printf "%s\n%!" (String.concat ~sep:"\n" (lines_of_ps lines_of_prog posprog))

(* prints ast using sexp format *)        
let dump_ast_sexp posprog = 
  Printf.printf "%s\n%!" (Sexp.to_string_hum ~indent:1 (Cool.sexp_of_posprog posprog))