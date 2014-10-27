%start <CoolValue option> program
%%

program:
  | EOF { None }
  | v = `INT_CONST { Some ( `Int(v) ) }
;



