let double x = 2 * x
let double_check x = double x = x + x

(* QCheck.int *)
let t = QCheck.Test.make ~count:1000 QCheck.int double_check

let _ = QCheck_runner.run_tests [t]

(* --------------------------------------- *)
let is_even n = n mod 2 = 0

let t = QCheck.Test.make (QCheck.make QCheck.Gen.int) is_even

let  _ = QCheck_runner.run_tests_main [t]

(* ------------------------------------------ *)
let rec is_sorted = function
  | [] -> true
  | [ _h ] -> true
  | h1 :: (h2 :: _t as t') -> h1 <= h2 && is_sorted t'


let is_sorted lst = lst = List.sort Stdlib.compare lst;;
let t = QCheck.(Test.make ~count:1000 (list small_nat) is_sorted);;
QCheck_runner.run_tests [t];;


(* --------------------------------------- *)

let is_sorted lst = lst = List.sort Stdlib.compare lst

let t =
  let open QCheck in
  Test.make ~count:1000 (list small_nat) is_sorted

let _ = QCheck_runner.run_tests [t]

(* --------------------------------------------- *)
open QCheck

let random_string_gen (st : Random.State.t) : string =
  let length = Random.State.int st 10 in 
  let chars = "abcdefghijklmnopqrstuvwxyz" in 
  let rec generate_string acc remaining_length =
    if remaining_length = 0 then acc
    else
      let random_char = String.get chars (Random.State.int st (String.length chars)) in
      generate_string (acc ^ String.make 1 random_char) (remaining_length - 1)
  in
  generate_string "" length 

let custom_string_gen = make random_string_gen 

let prop_length_twice s = String.length s * 2 = String.length (s ^ s) 

let _ =
  let test = Test.make ~count:1000 custom_string_gen prop_length_twice in
  QCheck_runner.run_tests [test];;
  
(* ---to do custom generation for a list to test the AVL Map--- *)
open QCheck

let random_int_list_arb : int list arbitrary =
  QCheck.make (Gen.(list_size (int_range 1 1000) small_int))

let prop_positive_integers lst =
  List.for_all (fun x -> x >= 0) lst

let test = Test.make ~count:100 random_int_list_arb prop_positive_integers

let _ = QCheck_runner.run_tests [test]
