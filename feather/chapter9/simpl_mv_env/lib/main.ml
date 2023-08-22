open Ast

(** [parse s] parses [s] into an AST *)
let parse (s:string) : expr =
  let lexbuf = Lexing.from_string s in 
  let ast = My_parser.prog My_lexer.read lexbuf in
  ast



(** [Env] is module to help with environments, which are maps
    that have strings as keys.  *)
module Env = Map.Make(String)

let empty_env = Env.empty

(** [env] is the type of an environment, 
    which maps a string  to a value *)
type env = value Env.t 

(** [value] is the type of a value.  *)
and value =
  | VInt of int
  | VBool of bool

(** The error message produced if binary operater and their operands
    do not have the correct type  *)
let bop_err = "Operator and operand type mismatch."

(** The error message produced if a variable is unbound. *)
let unbound_var_err = "Unbound variable"

(** The error message produced if the guard of [If] does not
    have type [Bool] *)
let if_guard_err = "Guard of if must have type Bool"

(** [eval_big env e] is the [v] such that <env, e> ==> v *)
let rec eval (env : env) (e: expr) : value = match e with
  | Int i  -> VInt i
  | Bool b -> VBool b
  | Var y -> eval_var env y
  | Binop (bop, e1, e2) -> eval_bop env bop e1 e2 
  | Let (x, e1, e2) -> eval_let env x e1 e2
  | If (e1, e2, e3) -> eval_if env e1 e2 e3

(** [eval_var] *)
and eval_var env x =
  try Env.find x env
  with Not_found -> failwith (unbound_var_err ^ " " ^ x)

and eval_if env e1 e2 e3 =
  match eval env e1 with
  | VBool true -> eval env e2
  | VBool false -> eval env e3
  | _ -> failwith if_guard_err

and eval_let env x e1 e2 =
  let v1 = eval env e1 in
  let env' = Env.add x v1 env in
  eval env' e2

and eval_bop env bop e1 e2 =
  match bop, eval env e1, eval env e2 with
  | Add, VInt a, VInt b -> VInt (a + b)
  | Mult, VInt a, VInt b -> VInt (a * b)
  | Leq, VInt a, VInt b -> VBool (a <= b)
  | _ -> failwith bop_err

(** [string_of_val e] convert e to a string,
    requires [e] is a value *)
let string_of_val (e: value) : string =
  match e with
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b 

let interp (s: string) : value =
  let e = parse s in
    eval empty_env e 

(*-------------------[primitive version]---------------------------*)

(** [string_of_eval e] convert e to a string,
    requires [e] is a value *)
let string_of_eval (e: expr) : string =
  match e with
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Var _ | Binop _ | Let _ | If _ -> failwith "not a value"  
  
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
let fix_expr_to_value = function
  | Int i -> VInt i
  | Bool b -> VBool b
  | _ -> failwith "not a value"
  
let rec eval_small (e: expr) : value =
  if is_value e then fix_expr_to_value e else
    e |> step |> eval_small

let interp_small (s:string) : value =
  let e = parse s in
    eval_small e
