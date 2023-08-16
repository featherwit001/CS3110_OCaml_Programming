
let rec from i j lst = 
  if i > j then lst else from i (j-1) (j :: lst)

let ( -- ) i j = from i j []
let long_lst = 1 -- 1_000_000

(** [take n lst] returns the first [n] elements of [lst]. 
   If [lst] has fewer than [n] elements, return all of them *)
let rec take n = function
  | _lst when n = 0 -> []
  | [] -> []
  | h :: t -> h :: take (n - 1) t

let rec take_aux acc n = function
  | _lst when n = 0 -> acc
  | [] -> acc
  | h :: t -> take_aux (h :: acc) (n - 1) t

let take n lst = List.rev (take_aux [] n lst)


(** [drop n lst] returns all but the first [n] elements of lst. 
    If [lst] has fewer than [n] elements, return the empty list.  *)
let rec drop n = function
  | lst when n = 0 -> lst
  | [] -> []
  | _h :: t -> drop (n - 1) t


(* type 'a tset_fun_input = int * ('a list) *)



(* let producetests test_name expect_output test_fun test_fun_input =
  match test_fun_input with
  | (n , lst) -> begin
    test_name >:: 
      (fun _ -> 
        assert_equal 
          expect_output 
          (test_fun n lst))
      end*)

open OUnit2

let take_tests = "take_tests" >::: [
  "take 0 in []" >:: (fun _ -> assert_equal [] (take 0 []));
  "take 1 in []" >:: (fun _ -> assert_equal [] (take 1 []));
  "take 3 in [1; 2]" >:: (fun _ -> assert_equal [1; 2] (take 3 [1; 2]));
  "take 3 in [1; 2]" >:: (fun _ -> assert_equal [1; 2; 3] (take 3 [1; 2; 3]));
  "take 3 in [1; 2; 3; 4; 5]" >:: 
          (fun _ -> assert_equal [1; 2; 3] (take 3 [1; 2; 3; 4; 5]));
  "take 1_000_000 in long_lst" >:: 
          (fun _ -> assert_equal long_lst (take 1_000_000 long_lst));
  "take 999_999 in long_lst" >:: 
          (fun _ -> assert_equal (1 -- 999_984) (take 999_984 long_lst));
]

let drop_tests = "drop_tests" >::: [
  "drop 0 in []" >:: (fun _ -> assert_equal [] (drop 0 []));
  "drop 1 in []" >:: (fun _ -> assert_equal [] (drop 1 []));
  "drop 3 in [1; 2]" >:: (fun _ -> assert_equal [] (drop 3 [1; 2]));
  "drop 3 in [1; 2; 3]" >:: (fun _ -> assert_equal [] (drop 3 [1; 2; 3]));
  "drop 2 in [1; 2; 3]" >:: 
          (fun _ -> assert_equal [3] (drop 2 [1; 2; 3]));
  "drop 1 in long_lst" >:: (fun _ -> assert_equal (2 -- 1_000_000) (drop 1 long_lst))
] 

let all_tests = "all_tests" >::: [take_tests; drop_tests]

let _ = run_test_tt_main  (all_tests) 
