{
open Lexing
open Core.Std
open Cool_parse

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    };;

let max_str_length = 1025;;
}

let classRegex = ['c''C'] ['l''L'] ['a''A'] ['s''S'] ['s''S']
let falseRegex = 'f' ['a''A'] ['l''L'] ['s''S'] ['e''E']
let white = [' ' '\t' '\011' '\012' '\013'] (* '\012' = '\r' *)
let elseRegex = ['e''E'] ['l''L'] ['s''S'] ['e''E']
let fi = ['f''F'] ['i''I']
let ifRegex = ['i''I'] ['f''F']
let inRegex= ['i''I'] ['n''N']
let inherits = ['i''I'] ['n''N'] ['h''H'] ['e''E'] ['r''R'] ['i''I'] ['t''T'] ['s''S']
let isvoid = ['i''I'] ['s''S'] ['v''V'] ['o''O'] ['i''I'] ['d''D']
let letrule = ['l''L'] ['e''E'] ['t''T']
let loop = ['l''L'] ['o''O'] ['o''O'] ['p''P']
let pool =  ['p''P'] ['o''O'] ['o''O'] ['l''L']
let thenRegex = ['t''T'] ['h''H'] ['e''E'] ['n''N']
let whileRegex = ['w''W'] ['h''H'] ['i''I'] ['l''L'] ['e''E']
let case = ['c''C'] ['a''A'] ['s''S'] ['e''E']
let esac = ['e''E'] ['s''S'] ['a''A'] ['c''C']
let newRegex = ['n''N'] ['e''E'] ['w''W']
let ofRegex = ['o''O'] ['f''F']
let notRegex = ['n''N'] ['o''O'] ['t''T']
let trueRegex = 't' ['r''R'] ['u''U'] ['e''E']
let intconst = ['0' - '9']+
let objid = [ 'a' - 'z' ]['a'-'z' 'A'-'Z' '0'-'9' '_']*
let typeid = [ 'A' - 'Z' ]['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule parse_escaped_char null buf = 
     parse
|   'n'   { Buffer.add_string buf "\n"; parse_string null buf lexbuf }
(*|   'r'   { Buffer.add_string buf "\r"; parse_string buf lexbuf } (* not accepted by cool*)*)
|   'b'	  { Buffer.add_string buf "\b"; parse_string null buf lexbuf }
|   't'	  { Buffer.add_string buf "\t"; parse_string null buf lexbuf }
|   'f'	  { Buffer.add_string buf "\012"; parse_string null buf lexbuf }
|   '\n'  { next_line lexbuf; Buffer.add_string buf "\n"; parse_string null buf lexbuf }
|   '\000' { parse_string true buf lexbuf }
|   eof   { ERROR("EOF in string constant") }
|   _ as x { Buffer.add_char buf x; parse_string null buf lexbuf }
and parse_string null buf = 
    parse
|   '\\'		{ parse_escaped_char null buf lexbuf }
|   '"'			{  if null then ERROR("String contains escaped null character.") else 
			     if Buffer.length buf >= max_str_length
			     then ERROR("String constant too long") 
			     else STR_CONST(Buffer.contents buf)
			}
|   '\n'	 	{ next_line lexbuf;  ERROR("Unterminated string constant");}
|   '\000'	 	{ parse_string true buf lexbuf }
|   eof 		{ ERROR("EOF in string constant") }
|   [^ '\\' '"' '\n' '\000' ]+  { Buffer.add_string buf (Lexing.lexeme lexbuf); parse_string null buf lexbuf }
|   _		     	{ failwith "never expected to reach this. string." }
and parse_single_line_comment =
     parse 
|    '\n'	{ next_line lexbuf; read lexbuf  }
|    eof 	{ EOF }
|    [^'\n']* 	{ parse_single_line_comment lexbuf }
|    _ 		{ failwith " should not have reached here. single line
     comment." }
and parse_multi_line_comment c = 
     parse
|    '*'		{ parse_multi_line_comment c lexbuf }
|    '*' ')'		{ if (c = 1) then read lexbuf else parse_multi_line_comment (c-1) lexbuf }
|    '('		{ parse_multi_line_comment c lexbuf } 
|    '(' '*'		{ parse_multi_line_comment (c + 1) lexbuf }
|    '\n'		{ next_line lexbuf; parse_multi_line_comment c lexbuf}
|    [^ '(' '*' '\n']* 	{ parse_multi_line_comment c lexbuf }
|    eof 		{ ERROR("EOF in multi line comment") }
|    _ 			{ failwith "should not have reached here. comment." }
and read = 
     parse 
|    white	{  read lexbuf }
|    classRegex	{ CLASS }
|    falseRegex	{ BOOL_CONST(bool_of_string (String.lowercase (Lexing.lexeme lexbuf))) }
|    elseRegex 	{ ELSE } 
|    fi		{ FI }
|    ifRegex	{ IF }
|    inRegex	{ IN }
|    inherits	{ INHERITS }
|    isvoid	{ ISVOID }
|    letrule	{ LET }
|    loop	{ LOOP }
|    pool	{ POOL }
|    thenRegex	{ THEN }
|    whileRegex	{ WHILE }
|    case	{ CASE }
|    esac	{ ESAC }
|    newRegex	{ NEW }
|    ofRegex	{ OF }
|    notRegex	{ NOT }
|    trueRegex	{ BOOL_CONST(bool_of_string (String.lowercase (Lexing.lexeme lexbuf))) }
|    "<-" 	{ ASSIGN }
|    "@"  	{ AT }
|    "."  	{ DOT }
|    ","  	{ COMMA }
|    "("  	{ LPAREN }
|    ")"  	{ RPAREN }
|    "{"  	{ LBRACE }
|    "}"  	{ RBRACE }
|    ":"  	{ COLON }
|    ";"  	{ SEMI }
|    "=>"  	{ DARROW } 
|    "*"  	{ MULT }
|    "+"  	{ PLUS }
|    "-"  	{ MINUS }
|    "/"  	{ DIV }
|    "~"  	{ NEG }
|    "<"  	{ LT }
|    "<="  	{ LE }
|    "="  	{ EQ }
|    "--" 	{ parse_single_line_comment lexbuf }
|    "(*" 	{ parse_multi_line_comment 1 lexbuf }
|    "*)"       { ERROR("Unmatched *)") }
|    intconst 	{ INT_CONST(Lexing.lexeme lexbuf) }
|    objid 	{ OBJECTID(Lexing.lexeme lexbuf) }
|    typeid 	{ TYPEID(Lexing.lexeme lexbuf) }
|    '"' 	{ parse_string false (Buffer.create 16) lexbuf }
|    '\n'	{ next_line lexbuf; read lexbuf }
|    eof 	{ lexbuf.lex_eof_reached <- true; EOF }
|    _		{ ERROR(Lexing.lexeme lexbuf) }

{

let list_of_lexbuf lexbuf b = 
    let toks = ref [] in
    (while b = lexbuf.lex_eof_reached do
    !toks <- (read lexbuf) :: (!toks)
    done); 
    List.rev !toks;;

let tokenize_str str = 
    let lexbuf = Lexing.from_string str in 
    list_of_lexbuf lexbuf true;;

let print_escaped_string str =
    (* this is meant to reproduce the functionality of Utilities.printEscapedString. *)
    let escape = function
    | '\\' -> "\\\\"
    | '\"' -> "\\\""
    | '\n' -> "\\n"
    | '\t' -> "\\t"
    | '\b' -> "\\b"
    (* | '\r' -> "\\r". (* see printEscapedString: they print "\r" as r *) *)
    | '\012' -> "\\f" (* ml does not use recognize  '\f' as a valid char *)
    | c -> let intc = (int_of_char c) in if  intc >= 0x20 && intc <= 0x7f  then (Char.to_string c) else (Printf.sprintf "\\%03o" intc)
    in String.concat (List.map (String.to_list str)  ~f:escape);;


let string_of_tok  = function 
    | ASSIGN -> "ASSIGN"
    | AT -> "'@'"
     | CASE -> "CASE"
     | CLASS -> "CLASS"
     | COLON -> "':'"
     | COMMA -> "','"
     | DARROW -> "DARROW"
     | DIV -> "'/'"
     | DOT -> "'.'"
     | ELSE -> "ELSE"
     | EOF -> "EOF"
     | EQ -> "'='"
     | ESAC -> "ESAC"
     | FI -> "FI"
     | IF -> "IF"
     | IN -> "IN"
     | INHERITS -> "INHERITS"
     | ISVOID -> "ISVOID"
     | LBRACE -> "'{'"
     | LE -> "LE"
     | LET -> "LET"
     | LOOP -> "LOOP"
     | LPAREN -> "'('"
     | LT -> "'<'"
     | MINUS -> "'-'"
     | MULT -> "'*'"
     | NEG -> "'~'"
     | NEW -> "NEW"
     | NOT -> "NOT"
     | OF -> "OF"
     | PLUS -> "'+'"
     | POOL -> "POOL"
     | RBRACE -> "'}'"
     | RPAREN -> "')'"
     | SEMI -> "';'"
     | THEN -> "THEN"
     | WHILE -> "WHILE"
     | BOOL_CONST(b) -> "BOOL_CONST " ^ string_of_bool(b)
     | INT_CONST(s) -> "INT_CONST " ^ s
     | OBJECTID(s)  -> "OBJECTID " ^ s
     | STR_CONST(s)  -> "STR_CONST \"" ^ (print_escaped_string s) ^ "\""
     | TYPEID(s) -> "TYPEID " ^  s
     | ERROR(s) -> "ERROR \"" ^ (print_escaped_string s) ^ "\""

(* convenience method to shorten return type *)
let read_token lexbuf : token = read lexbuf;;

(*
* tokenizes from the inchannel, prints tokens line by line into outchannel.
*)
let tokenize_from_to inch outch   =
    let lexbuf = Lexing.from_channel inch in
    (while not lexbuf.lex_eof_reached do
       let tok = read lexbuf in
       let line = lexbuf.lex_curr_p.pos_lnum in
       if (tok <> EOF) then Printf.fprintf outch "#%d %s\n" line (string_of_tok tok)
       done);;

(*
* tokenizes the contents of the file with filename,
* and writes the contents into filename.out.actual.
*)
let tokenize_file infile  =
    let outfile = infile ^ ".out.actual"  in 
    let outch = Out_channel.create outfile in 
    let inch = In_channel.create infile in 
    Printf.fprintf outch "#name \"%s\"\n" infile; 
    tokenize_from_to inch outch;;


}
