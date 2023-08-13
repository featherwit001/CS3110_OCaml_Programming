open OUnit2
open Sum


let make_sum_test name expected_output input =
  name >:: (fun _ctxt -> 
    assert_equal expected_output (sum input) ~printer:string_of_int)

let tests1 = "test suite for sum" >::: [
  make_sum_test "empty" 0 [];
  make_sum_test "singleton" 1 [1];
  make_sum_test "two_elements" 3 [1; 2];
]

open OUnit2

let tests2 = "suite" >::: [
    "empty" >:: (fun _ -> assert_raises (Failure "hd") (fun () -> List.hd []));
  ]

let _ = run_test_tt_main ("all_test" >::: [tests1; tests2])

(* dune build test.exe *)
(* dune exec ./test.exe *)
