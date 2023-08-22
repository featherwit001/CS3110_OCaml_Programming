(** The type of binary operators. *)
type bop = 
  | Add
  | Mult
  | Leq (*<=*)

(** The type of the abstract syntax tree (AST). *)
type expr =
  | Var of string
  | Int of int
  | Bool of bool  (* false and true*)
  | App of expr * expr
  | Fun of string * expr
  | Binop of bop * expr * expr
  | Let of string * expr * expr
  | If of expr * expr * expr
