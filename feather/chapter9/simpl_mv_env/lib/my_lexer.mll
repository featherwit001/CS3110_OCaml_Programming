{
  open My_parser
  exception Exit_calc
}

let white = ['\x20' '\x09' '\x0A' '\x0D']*
(* let white = [' ' '\t' '\n']+ *)
let digit = ['0'-'9'] 
let int = digit+
let letter = ['a'-'z' 'A'-'Z']
let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = 
  parse
  | white { read lexbuf }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "true" { TRUE }
  | "false" { FALSE }
  | "<=" { LEQ }
  | "*" { TIMES }
  | "+" { PLUS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "let" { LET }
  | "=" { EQUALS }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "->" { RARROW }
  | "fun"{ FUN }
  | ";" { SEMICOLON }
  | ";;" { DSEMICOLON }
  | ":" { COLON }
  | "int" { INT_TYPE }
  | "bool" { BOOL_TYPE }
  | id { ID (Lexing.lexeme lexbuf) }
  (* useless *)
  (* | "_exit" {raise  Exit_calc} *)
  | eof { EOF }

