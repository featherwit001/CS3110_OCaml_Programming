{
  open My_parser
  exception Exit_calc
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'?digit+

rule read =
  parse
(* recsive read to skip white and return next token *)
  | white { read lexbuf }  
  | "*" { TIMES }
  | "+" { PLUS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "_exit" {raise  Exit_calc}
  | eof { EOF }

