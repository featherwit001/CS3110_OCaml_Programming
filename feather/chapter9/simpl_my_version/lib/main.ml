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
  | Bool b -> string_of_bool b 
  | Var _ | Binop _ | Let _ | If _ -> failwith "precondition violated"

(** [is_value e] is whether [e] is a value *)
let is_value : expr -> bool = function
  | Int _ | Bool _ -> true
  | Var _ | Binop _ | Let _ | If _  -> false

(** [subst e v x] is [e] with [v] substituted for [x], that is 
    e {v/x}
    Example: subst (Var "x" + Var "y") v "x"  = v + Var "y" *)
let rec subst e v x = 
  match e with
  | Var y -> if x = y then v else e
  | Int _  -> e
  | Bool _ -> e
  | Binop (bop, e1, e2) -> Binop (bop, subst e1 v x, subst e2 v x)
  | Let (y, e1, e2) -> if x = y then Let (y, subst e1 v x, e2)
                                else Let (y, subst e1 v x, subst e2 v x)  
  | If (e1, e2, e3) -> If (subst e1 v x, subst e2 v x, subst e3 v x)

(** The error message produced if binary operater and their operands
    do not have the correct type  *)
let bop_err = "Operator and operand type mismatch."

(** The error message produced if a variable is unbound. *)
let unbound_var_err = "Unbound variable"

(** The error message produced if the guard of [If] does not
    have type [Bool] *)
let if_guard_err = "Guard of if must have type Bool"


(** [step e] is a single step of evaluation of [e] *)
let rec step : expr -> expr  = function
  | Var y -> failwith (unbound_var_err ^ " " ^ y)
  | Int _ | Bool _-> failwith "Does not step"
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 -> 
                           step_bop bop e1 e2 
  | Binop (bop, e1, e2) when is_value e1 -> 
                           Binop (bop, e1, step e2)
  | Binop (bop, e1, e2) -> Binop (bop, step e1, e2)
  | Let (x, e1, e2) when is_value e1 -> subst e2 e1 x
  | Let (x, e1, e2) -> Let (x, step e1, e2)
  | If (e1, e2, e3) when is_value e1 -> step_if e1 e2 e3
  | If (e1, e2, e3) -> If (step e1, e2, e3)

(** [step_if v e2 e3] return e2 or e3 
    Require [v] is Boolean value  *)
and step_if v e2 e3 = match v with
  | Bool true -> e2
  | Bool false -> e3
  | Int _ -> failwith if_guard_err 
  | _ -> failwith "precondition violated"

(** [step_bop bop e1 e2] implement the primitive operation
    [v1 bop v2]; Require: [v1] and [v2] are both values.  *)
and step_bop bop e1 e2 = 
  match bop, e1, e2 with
  | Add, Int a, Int b -> Int (a + b)
  | Add, _, _ -> failwith bop_err
  | Mult, Int a, Int b -> Int (a * b)
  | Mult, _, _ -> failwith bop_err
  | Leq, Int a, Int b -> Bool (a <= b)
  | Leq, _, _ -> failwith bop_err


let rec eval_big (e: expr) : expr = match e with
  | Int _ | Bool _ -> e
  | Var y -> failwith (unbound_var_err ^ " " ^ y)
  | Binop (bop, e1, e2) -> eval_bop bop e1 e2 
  | Let (x, e1, e2) -> eval_let x e1 e2
  | If (e1, e2, e3) -> eval_if e1 e2 e3

and eval_if e1 e2 e3 =
  match eval_big e1 with
  | Bool true -> eval_big e2
  | Bool false -> eval_big e3
  | _ -> failwith if_guard_err

and eval_let x e1 e2 =
  let v1 = eval_big e1 in
  let e2' = subst e2 v1 x in
  eval_big e2'

and eval_bop bop e1 e2 =
  match bop, eval_big e1, eval_big e2 with
  | Add, Int a, Int b -> Int (a + b)
  | Mult, Int a, Int b -> Int (a * b)
  | Leq, Int a, Int b -> Bool (a <= b)
  | _ -> failwith bop_err

let interp_big (s: string) : expr =
  let e = parse s in
    eval_big e 


(** [eval e] fully evaluate [e] => value [v]  *)
let rec eval (e : expr) : expr = 
  if is_value e then e else
    e |> step |> eval

(** [linterp s] interprets [s] by lexing and parsing it,
    evaluating it, and converting the result to a string *)
let interp (s: string) : string =
  s |> parse |> eval |> string_of_val

let rec eval_small (e: expr) : expr =
  if is_value e then e else
    e |> step |> eval_small

let interp_small (s:string) : expr =
  let e = parse s in
    eval_small e
