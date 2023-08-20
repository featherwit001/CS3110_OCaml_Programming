type bop =
  | Add
  | Mul
  | MINUS
  | DIV

type expr = 
  | Int of int
  | Float of float
  | Binop of bop * expr * expr
