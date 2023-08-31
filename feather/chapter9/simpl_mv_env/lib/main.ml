open Ast


exception TypeError of string
exception RuntimeError of string

let type_error s =
  raise (TypeError s)

let runtime_error s =
  raise (RuntimeError s)


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
  | Closure of string * expr * env


type scope_rule = Lexical | Dynamic

let scope = ref Lexical


(** The error message produced if binary operater and their operands
    do not have the correct type  *)
let bop_err = "Operator and operand type mismatch."

(** The error message produced if a variable is unbound. *)
let unbound_var_err = "Unbound variable"

(** The error message produced if the guard of [If] does not
    have type [Bool] *)
let if_guard_err = "Guard of if must have type Bool"

(** The error message produced if   does not a function *)
let apply_err = "not a function"

let string_unable_to_print = "<abstr>"

(** The error message produced if the binding expression of a [let]
    does not have the same type as the annotation on the variable name  *)
let annotation_error = "Let expreesion type mismatch"

(** The error message produced if the two branches of an [if]
    do not have the same type  *)
let if_branch_err = "Branches of if must have the same type "

(** [empty_typ_env] is the empty environment *)
let empty_typ_env = []


(** [extend env x t] is [env] extended with a binding of
    [x] to [t].  *)
let extend env x t =
  (x, t) :: env


(** [lookup env e] is the type of [e] in the environment [env]
    Raises: [Failure] if [e] is not bound in [env]  *)
let lookup env e =
  match List.assoc_opt e env with
  | Some t -> t 
  | None -> type_error (unbound_var_err ^ " " ^ e)


(** [typeof env e] is the type of [e] in environment [env]
    That is, it is the [t] such that [env |- e : t]  
    Raises: [Failure] if not such [t] exists*)
let rec typeof env e =
  match e with
  | Bool _ -> TBool
  | Int _ -> TInt
  | Var x -> lookup env x
  | Binop (bop, e1, e2) -> typeof_binop env bop e1 e2
  | Let (x, t, e1, e2) -> typeof_let env x t e1 e2
  | If (e1, e2, e3) -> typeof_if env e1 e2 e3 
  | _ -> failwith "to do"

(** [typeof_binop env bop e1 e2] is the type of [e1 binop e2] in
    enironment [env]  *)
and typeof_binop env bop e1 e2 = 
    match bop, typeof env e1, typeof env e2 with
    | Add, TInt, TInt -> TInt
    | Mult, TInt, TInt -> TInt
    | Leq, TInt, TInt -> TBool
    | _ -> type_error bop_err 

(** [typeof_let env x t e1 e2] is the type of [let x : t = e1 in e2]
    in environment [env]   *)
and typeof_let env x t e1 e2 = 
    let t' = typeof env e1 in
      if t = t' then
        let env' = extend env x t' in
        typeof env' e2
      else
        type_error annotation_error

and typeof_if env e1 e2 e3 = 
    let t1 = typeof env e1 in
    if t1 <> TBool then
      type_error if_guard_err  
    else 
      let t2, t3 = typeof env e2, typeof env e3 in
      if t2 <> t3 then type_error if_branch_err
      else t3
      

(** [typecheck e] is [e] if [e] typechecks, that is if there
    [t]  such that  [{} |- e : t]
    Raises: [Failure] if  [e] does not type check. *)
let typecheck e = 
    ignore (typeof empty_typ_env e);
    e


(** [eval_big env e] is the [v] such that <env, e> ==> v *)
let rec eval (env : env) (e: expr) : value = match e with
  | Int i  -> VInt i
  | Bool b -> VBool b
  | Var y -> eval_var env y
  | Binop (bop, e1, e2) -> eval_bop env bop e1 e2 
  | Let (x, _t, e1, e2) -> eval_let env x e1 e2
  | If (e1, e2, e3) -> eval_if env e1 e2 e3
  | App (e1, e2) -> eval_app env e1 e2
  | Fun (x, e) -> Closure (x, e, env)

and eval_app env e1 e2 =
  match eval env e1 with
  | Closure (x, e, defenv) -> begin
    let v2 = eval env e2 in
    let base_env_for_body =
      match !scope with
      | Lexical -> defenv
      | Dynamic -> env in
      let env_for_body = Env.add x v2 base_env_for_body in
      eval env_for_body e
  end
  | _ -> failwith apply_err

(** [eval_var] *)
and eval_var env x =
  try Env.find x env
  with Not_found -> runtime_error (unbound_var_err ^ " " ^ x)

and eval_if env e1 e2 e3 =
  match eval env e1 with
  | VBool true -> eval env e2
  | VBool false -> eval env e3
  | _ -> runtime_error if_guard_err

and eval_let env x e1 e2 =
  let v1 = eval env e1 in
  let env' = Env.add x v1 env in
  eval env' e2

and eval_bop env bop e1 e2 =
  match bop, eval env e1, eval env e2 with
  | Add, VInt a, VInt b -> VInt (a + b)
  | Mult, VInt a, VInt b -> VInt (a * b)
  | Leq, VInt a, VInt b -> VBool (a <= b)
  | _ -> runtime_error bop_err (* match inexhuasted*)

(** [string_of_val e] convert e to a string,
    requires [e] is a value *)
let string_of_val (e: value) : string =
  match e with
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b 
  | _ -> string_unable_to_print

let interp (s: string) : value =
  let e = parse s in
  (* remove typecheck test/main.ml could run all_tests *)
  ignore(typecheck e);
  eval empty_env e 


(*-------------------[primitive version]---------------------------*)
(*-------------------[     Discard     ]---------------------------*)

(** [string_of_eval e] convert e to a string,
    requires [e] is a value *)
let string_of_eval (e: expr) : string =
  match e with
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Fun _ -> string_unable_to_print
  | Var _ | Binop _ | Let _ | If _ | App _  -> failwith "not a value"  
  
(** [is_value e] is whether [e] is a value *)
let is_value : expr -> bool = function
  | Int _ | Bool _ | Fun _-> true
  | Var _ | Binop _ | Let _ | If _| App _  -> false

(** [subst e v x] is [e] with [v] substituted for [x], that is 
    e {v/x}
    Example: subst (Var "x" + Var "y") v "x"  = v + Var "y" 
    Discard*)
let rec subst e v x = 
  match e with
  | Var y -> if x = y then v else e
  | Int _  -> e
  | Bool _ -> e
  | Binop (bop, e1, e2) -> Binop (bop, subst e1 v x, subst e2 v x)
  | Let (y, t, e1, e2) -> if x = y then Let (y, t, subst e1 v x, e2)
                                else Let (y, t, subst e1 v x, subst e2 v x)  
  | If (e1, e2, e3) -> If (subst e1 v x, subst e2 v x, subst e3 v x) 
  | _ -> failwith "never to implement" 

(** [step e] is a single step of evaluation of [e] *)
let rec step : expr -> expr  = function
  | Var y -> failwith (unbound_var_err ^ " " ^ y)
  | Int _ | Bool _-> failwith "Does not step"
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 -> 
                           step_bop bop e1 e2 
  | Binop (bop, e1, e2) when is_value e1 -> 
                           Binop (bop, e1, step e2)
  | Binop (bop, e1, e2) -> Binop (bop, step e1, e2)
  | Let (x, _t, e1, e2) when is_value e1 -> subst e2 e1 x
  | Let (x, t, e1, e2) -> Let (x, t, step e1, e2)
  | If (e1, e2, e3) when is_value e1 -> step_if e1 e2 e3
  | If (e1, e2, e3) -> If (step e1, e2, e3)
  | _ -> failwith "never to implement" 
(* fun could not implemented lexical scope using step and substitute *)

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
