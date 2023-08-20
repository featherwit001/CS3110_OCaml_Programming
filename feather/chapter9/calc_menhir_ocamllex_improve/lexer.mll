{
  open My_parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
(* Negation symbol and minus sign 
   can be easily confused during recognition.*)
let int = digit+
let float = digit+"."digit*

rule read =
  parse
(* recsive read to skip white and return next token *)
  | white { read lexbuf }  
  | float {FLOAT (float_of_string (Lexing.lexeme lexbuf))}
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "*" { TIMES }
  | "+" { PLUS }
  | "-" { MINUS }
  | "/" { DIV }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | eof { EOF }

