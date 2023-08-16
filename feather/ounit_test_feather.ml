exception Empty

module type Stack = sig
  type 'a t
  val empty : 'a t
  val push : 'a -> 'a t -> 'a t
  val peek : 'a t -> 'a
  val pop : 'a t -> 'a t
end

module ListStack = struct
  type 'a t = 'a list
  let empty = []
  let push = List.cons
  let peek = function [] -> raise Empty | x :: _ -> x
  let pop = function [] -> raise Empty | _ :: s -> s
end

module VariantStack = struct
  type 'a t = E | S of 'a * 'a t
  let empty = E
  let push x s = S (x, s)
  let peek = function E -> raise Empty | S (x, _) -> x
  let pop = function E -> raise Empty | S (_, s) -> s
end

(* write a first-class module for test *)



open OUnit2

module StackTester (M : Stack) = struct
  let tests = [
    "tests for push and peek" >:: (fun _ -> assert_equal 1 M.(empty |> push 1 |> peek))
  ]
    
end

let stacks = [(module ListStack: Stack); (module VariantStack: Stack)]

let all_test = 
  let producetests m = 
    let module M = (val m: Stack) in
    let module T = StackTester(M) in
     T.tests
    in
  let open List in
  flatten (map producetests stacks)

let _ = run_test_tt_main ("all_test" >::: all_test)

