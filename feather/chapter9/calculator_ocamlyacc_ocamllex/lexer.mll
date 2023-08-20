{
  open My_parser
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
  | eof { EOF }

