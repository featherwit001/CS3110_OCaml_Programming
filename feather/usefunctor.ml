
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

open OUnit2

(* functor input a module output this module's tests *)
module StackTester (S : Stack) = struct
  let tests = [
    "peek (push x empty) = x" >:: fun _ ->
      assert_equal 1 S.(empty |> push 1 |> peek)
  ]
end

module ListStackTester = StackTester (ListStack)
module VariantStackTester = StackTester (VariantStack)

let all_tests = List.flatten [
  ListStackTester.tests;
  VariantStackTester.tests
]

(* first-class module *)
let stacks = [(module ListStack: Stack); (module VariantStack)]

(* all_test = map StackTester stacks  and .tests *)
let all_tests = 
  let tests m =
    let module S = (val m : Stack) in 
    let module T = StackTester(S) in
    T.tests
  in
  let open List in 
  stacks |> map tests |> flatten

let testsuit = "all test" >::: all_tests

let _ = run_test_tt_main testsuit


(* wrap module into first-class module and unwrapped *)

module type MyModuleType = sig
  val sayhello : string -> string
end

module MyModule : MyModuleType = struct
  let sayhello name = "hello " ^ name
end

let my_first_class_module = (module MyModule: MyModuleType)


let () =
  let module M = (val my_first_class_module: MyModuleType) in
  let result = M.sayhello "wamgyx" in
  print_endline result