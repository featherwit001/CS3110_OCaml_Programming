open Ast

(** [parse s] parses [s] into an AST *)
let parse (s:string) : expr =
  let lexbuf = Lexing.from_string s in 
  let ast = My_parser.prog My_lexer.read lexbuf in
  ast

(** [string_of_val e] convert e to a string,
    requires [e] is a value *)
let string_of_val (e: expr) : string =
  match e with
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Binop _ -> failwith "precondition violated"

(** [is_value e] is whether [e] is a value *)
let is_value : expr -> bool = function
  | Int _ -> true
  | Float _ -> true
  | Binop _ -> false

(** [step e] is a single step of evaluation of [e] *)
let rec step : expr -> expr  = function
  | Int _ -> failwith "Does not step"
  | Float _ -> failwith "Does not step"
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 -> 
    step_bop bop e1 e2 
  | Binop (bop, e1, e2) when is_value e1 -> Binop (bop, e1, step e2)
  | Binop (bop, e1, e2) -> Binop (bop, step e1, e2)

(** [step_bop bop e1 e2] implement the primitive operation
    [v1 bop v2]; Require: [v1] and [v2] are both values.  *)
and step_bop bop e1 e2 = 
  match bop, e1, e2 with
  | Add, Int a, Int b -> Int (a + b)
  | Add, Float a, Float b -> Float (a +. b)
  | Add, Float a, Int b  -> Float (a +. float_of_int b)
  | Add, Int a, Float b ->Float (float_of_int a +.  b)

  | Mul, Int a, Int b -> Int (a * b)
  | Mul, Float a, Float b -> Float (a *. b)
  | Mul, Float a, Int b  -> Float (a *. float_of_int b)
  | Mul, Int a, Float b ->Float (float_of_int a *.  b)
  
  | MINUS, Int a, Int b -> Int (a - b)
  | MINUS, Float a, Float b -> Float (a -. b)
  | MINUS, Float a, Int b  -> Float (a -. float_of_int b)
  | MINUS, Int a, Float b ->Float (float_of_int a -.  b)
  
  | DIV, Int a, Int b -> 
          let c = a /b in 
          if c * b = a then Int (a / b) 
          else Float (float_of_int a /. float_of_int b)
  | DIV, Float a, Float b -> Float (a /. b)
  | DIV, Float a, Int b  -> Float (a /. float_of_int b)
  | DIV, Int a, Float b ->Float (float_of_int a /.  b)
  
  | Add, _, _ -> failwith "precondition violated"
  | Mul, _, _ -> failwith "precondition violated"
  | MINUS, _, _ -> failwith "precondition violated"
  | DIV, _, _ -> failwith "precondition violated"



(** [eval e] fully evaluate [e] => value [v]  *)
let rec eval (e : expr) : expr = 
  if is_value e then e else
    e |> step |> eval

(** [linterp s] interprets [s] by lexing and parsing it,
    evaluating it, and converting the result to a string *)
let interp (s: string) : string =
  s |> parse |> eval |> string_of_val


