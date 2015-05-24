open Cool;;

val err_count: unit -> int;;
val deflatten: Cool.letrec -> Cool.letrec ;;
val syntax_error: Cool.lexpos ->  int -> string -> unit;;
val set_debug: unit -> unit;;
val debug_print: string -> unit;;
val untyped_expr: expr -> Cool.lexpos -> posexpr
