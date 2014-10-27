%token <int> INT_CONST
%start <Cool.node> program

%%

program:
  | v = INT_CONST { `Int(v) }
;



