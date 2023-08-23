{
  open My_parser
  exception Exit_calc
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

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
  | id { ID (Lexing.lexeme lexbuf) }
  | "_exit" {raise  Exit_calc}
  | eof { EOF }

