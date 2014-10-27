/*	
 *  The scanner definition for COOL.
 */

/*
// Notes for the Java Version of the Assignment

// • Each call on the scanner returns the next token and lexeme from the input. The value returned by the
// method CoolLexer.next token is an object of class java cup.runtime.Symbol. This object has
// a field representing the syntactic category of a token (e.g., integer literal, semicolon, the if keyword,
// etc.). The syntactic codes for all tokens are defined in the file TokenConstants.java. The component,
// the semantic value or lexeme (if any), is also placed in a java cup.runtime.Symbol object. The
// documentation for the class java cup.runtime.Symbol as well as other supporting code is available
// on the course web page. Examples of its use are also given in the skeleton.

// • For class identifiers, object identifiers, integers, and strings, the semantic value should be of type
// AbstractSymbol. For boolean constants, the semantic value is of type java.lang.Boolean. Except
// for errors (see below), the lexemes for the other tokens do not carry any interesting information. Since
// the value field of class java cup.runtime.Symbol has generic type java.lang.Object, you will need
// to cast it to a proper type before calling any methods on it.

// • We provide you with a string table implementation, which is defined in AbstractTable.java. For the
// moment, you only need to know that the type of string table entries is AbstractSymbol.

// • When a lexical error is encountered, the routine CoolLexer.next token should return a
// java cup.runtime.Symbol object whose syntactic category is TokenConstants.ERROR and
// whose semantic value is the error message string. See Section 4 for information on how to construct
// error messages.
*/


import java_cup.runtime.Symbol;
 /* keywords: go first to be eagaerly matched*/
 /*
The keywords of cool are: class, else, false, fi, if, in, inherits, isvoid, let, loop, pool, then, while,
case, esac, new, of, not, true. Except for the constants true and false, keywords are case insensitive.
To conform to the rules for other objects, the first letter of true and false must be lowercase; the trailing
letters may be upper or lower case.
 */

%%

%{

/*  Stuff enclosed in %{ %} is copied verbatim to the lexer class
 *  definition, all the extra variables/functions you want to use in the
 *  lexer actions should go here.  Don't remove or modify anything that
 *  was there initially.  */

    // Max size of string constants
    static int MAX_STR_CONST = 1025;

    // For assembling string constants
    StringBuffer string_buf = new StringBuffer();
    boolean string_valid = true;

    private int curr_lineno = 1;
    int get_curr_lineno() {
	return curr_lineno;
    }

    private AbstractSymbol filename;

    void set_filename(String fname) {
	filename = AbstractTable.stringtable.addString(fname);
    }

    AbstractSymbol curr_filename() {
	return filename;
    }
    
    private int comment_depth = 0;
%}

%init{

/*  Stuff enclosed in %init{ %init} is copied verbatim to the lexer
 *  class constructor, all the extra initialization you want to do should
 *  go here.  Don't remove or modify anything that was there initially. */

    // empty for now
%init}

%eofval{

/*  Stuff enclosed in %eofval{ %eofval} specifies java code that is
 *  executed when end-of-file is reached.  If you use multiple lexical
 *  states and want to do something special if an EOF is encountered in
 *  one of those states, place your code in the switch statement.
 *  Ultimately, you should return the EOF symbol, or your lexer won't
 *  work.  */

    switch(yy_lexical_state) {
    case YYINITIAL:
	/* nothing special to do in the initial state */
	break;
	/* If necessary, add code for other states here, e.g:
	   case COMMENT:
	   ...
	   break;
	*/
    case MULTI_LINE_COMMENT:
    	yybegin(YYINITIAL);
		return new Symbol(TokenConstants.ERROR, new String("EOF in comment"));	
    case STRING:
        yybegin(YYINITIAL);
		return new Symbol(TokenConstants.ERROR, new String("EOF in string constant"));
    }
    return new Symbol(TokenConstants.EOF);
%eofval}

%class CoolLexer
%cup
%state SINGLE_LINE_COMMENT
%state MULTI_LINE_COMMENT
%state STRING
%state ESCAPE
%state STRING_ERROR_RECOVERY
%%

<YYINITIAL>[c|C][l|L][a|A][s|S][s|S] {
    return new Symbol(TokenConstants.CLASS);
}

<YYINITIAL>[e|E][l|L][s|S][e|E] {
    return new Symbol(TokenConstants.ELSE);
}

<YYINITIAL>f[a|A][l|L][s|S][e|E] {
    return new Symbol(TokenConstants.BOOL_CONST, new Boolean(false));
}

<YYINITIAL>[f|F][i|I] {
    return new Symbol(TokenConstants.FI);
}

<YYINITIAL>[i|I][f|F] {
    return new Symbol(TokenConstants.IF);
}

<YYINITIAL>[i|I][n|N] {
    return new Symbol(TokenConstants.IN);
}

<YYINITIAL>[i|I][n|N][h|H][e|E][r|R][i|I][t|T][s|S] {
    return new Symbol(TokenConstants.INHERITS);
}

<YYINITIAL>[i|I][s|S][v|V][o|O][i|I][d|D] {
    return new Symbol(TokenConstants.ISVOID);
}

<YYINITIAL>[l|L][e|E][t|T] {
    return new Symbol(TokenConstants.LET);
}

<YYINITIAL>[l|L][o|O][o|O][p|P] {
    return new Symbol(TokenConstants.LOOP);
}

<YYINITIAL>[p|P][o|O][o|O][l|L] {
    return new Symbol(TokenConstants.POOL);
}

<YYINITIAL>[t|T][h|H][e|E][n|N] {
    return new Symbol(TokenConstants.THEN);
}

<YYINITIAL>[w|W][h|H][i|I][l|L][e|E] {
    return new Symbol(TokenConstants.WHILE);
}

<YYINITIAL>[c|C][a|A][s|S][e|E] {
    return new Symbol(TokenConstants.CASE);
}

<YYINITIAL>[e|E][s|S][a|A][c|C] {
    return new Symbol(TokenConstants.ESAC);
}

<YYINITIAL>[n|N][e|E][w|W] {
    return new Symbol(TokenConstants.NEW);
}

<YYINITIAL>[o|O][f|F] {
    return new Symbol(TokenConstants.OF);
}

<YYINITIAL>[n|N][o|O][t|T] {
    return new Symbol(TokenConstants.NOT);
}

<YYINITIAL>t[r|R][u|U][e|E] {
    return new Symbol(TokenConstants.BOOL_CONST, new Boolean(true));
}

<YYINITIAL>"<-" {
  return new Symbol(TokenConstants.ASSIGN);
}

<YYINITIAL>"@" {
  return new Symbol(TokenConstants.AT);
}

<YYINITIAL>"." {
  return new Symbol(TokenConstants.DOT);
}

<YYINITIAL>"," {
  return new Symbol(TokenConstants.COMMA);
}

<YYINITIAL>"(" {
  return new Symbol(TokenConstants.LPAREN);
}

<YYINITIAL>")" {
  return new Symbol(TokenConstants.RPAREN);
}

<YYINITIAL>"{" {
  return new Symbol(TokenConstants.LBRACE);
}

<YYINITIAL>"}" {
  return new Symbol(TokenConstants.RBRACE);
}

<YYINITIAL>":" {
  return new Symbol(TokenConstants.COLON);
}

<YYINITIAL>";" {
  return new Symbol(TokenConstants.SEMI);
}

<YYINITIAL>"=>" { 
  /* Sample lexical rule for "=>" arrow.
     Further lexical rules should be defined
     here, after the last %% separator */
  return new Symbol(TokenConstants.DARROW); 
 }

<YYINITIAL>"*" {
  return new Symbol(TokenConstants.MULT);
}

<YYINITIAL>"+" {
  return new Symbol(TokenConstants.PLUS);
}

<YYINITIAL>"-" {
  return new Symbol(TokenConstants.MINUS);
}

<YYINITIAL>"/" {
  return new Symbol(TokenConstants.DIV);
}

<YYINITIAL>"~" {
  return new Symbol(TokenConstants.NEG);
}

<YYINITIAL>"<" {
  return new Symbol(TokenConstants.LT);
}

<YYINITIAL>"<=" {
  return new Symbol(TokenConstants.LE);
}

<YYINITIAL>"=" {
  return new Symbol(TokenConstants.EQ);
}

<YYINITIAL>"*)" {
  return new Symbol(TokenConstants.ERROR, "unmatched *)");
 }

<YYINITIAL>"--" {
  yybegin(SINGLE_LINE_COMMENT);
 }

<SINGLE_LINE_COMMENT>[^\n]* {}

<SINGLE_LINE_COMMENT>\n {
  ++curr_lineno;
  yybegin(YYINITIAL);
}

<YYINITIAL>"(*" {
  yybegin(MULTI_LINE_COMMENT);
  ++comment_depth;
 }

<MULTI_LINE_COMMENT>"(*" {
  ++comment_depth;
}

<MULTI_LINE_COMMENT>"*)" {
	--comment_depth;
	if (comment_depth == 0) {
		yybegin(YYINITIAL);
	}
 }
 
<MULTI_LINE_COMMENT>\n {
	++curr_lineno;
}
 
<MULTI_LINE_COMMENT>[^*(\n]* {}

<MULTI_LINE_COMMENT>"*" { 
	//loses to \*\) in length 
}

<MULTI_LINE_COMMENT>"(" { 
	//loses to "(*" in length 
}

<YYINITIAL>\" {
    yybegin(STRING);
    string_valid = true;
    string_buf = new StringBuffer();
 }

<STRING>[^\\\"\n\0]* {
	// up to first escape, or endquote, or newline or illegal char.
	string_buf.append(yytext());
}

<STRING>\\.|\\\n {
	// first escape
	// in jlex, dot means everything but newline.
	String x = yytext();
	assert(x.length() == 2);
		
	switch (x.charAt(1)){
		case 'n':
		   string_buf.append('\n');
		   break;
		case 'b':
		   string_buf.append('\b');
		   break;
		case 't':
		   string_buf.append('\t');
		   break;
		case 'f':
		   string_buf.append('\f');
		   break;
		case '\0':
			string_valid = false;
			break;
		case '\n':
			++curr_lineno;
			//drops down to default case
		default:
			string_buf.append(x.charAt(1));
	}
}

<STRING>\n {
	// a newline ends the string.
		++curr_lineno;	
		yybegin(YYINITIAL);
		return new Symbol(TokenConstants.ERROR, "Unterminated string constant") ;
}

<STRING>\0 {
	// a null makes the string invalid, but we aren't done yet. need to stay in string state.
	string_valid = false;
}

<STRING>\" {
	// end of string
	yybegin(YYINITIAL);
	if (string_buf.length() < MAX_STR_CONST && string_valid){
		return new Symbol(TokenConstants.STR_CONST, new StringSymbol(string_buf.toString(), string_buf.toString().length(), 0));
	} else if (string_valid){
		return new Symbol(TokenConstants.ERROR, "String constant too long");	
	} else if (!string_valid){
		return new Symbol(TokenConstants.ERROR, "String contains null character");
	} else {
		assert(false);
	}
}

<YYINITIAL>\n {
	++curr_lineno;
}

<YYINITIAL>[ \t\n\f\r\013] {
 /* \v is \013, but \v doesn't seem to work as well */
 }

<YYINITIAL>[0-9]+ { 
  //FIXME: index
  return new Symbol(TokenConstants.INT_CONST, new IntSymbol(yytext(), yytext().length(), 0));
}

<YYINITIAL>[a-z][a-zA-Z0-9_]* {
  return new Symbol(TokenConstants.OBJECTID, new IdSymbol(yytext(), yytext().length(), 0));
}

<YYINITIAL>[A-Z][a-zA-Z0-9_]* {
  return new Symbol(TokenConstants.TYPEID, new IdSymbol(yytext(), yytext().length(), 0));
}

.                               { /* This rule should be the very last
                                     in your lexical specification and
                                     will match match everything not
                                     matched by other lexical rules. */
                                  	return new Symbol(TokenConstants.ERROR, yytext());
                                }
