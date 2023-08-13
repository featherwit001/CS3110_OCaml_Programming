open OUnit2
(* [take n lst] returns the first [n] elements of [lst]. 
   If [lst] has fewer than [n] elements, return all of them *)
let rec take n  = function
  | lst when List.length lst <= n -> lst
  | h :: t -> h :: take (n - 1) t
  | _ -> failwith "UnImpl"

  (** [drop n lst] returns all but the first [n] elements of lst. 
      If [lst] has fewer than [n] elements, return the empty list.  *)
let rec _drop n = function
  | lst when List.length lst <= n -> []
  | _h :: t -> _drop (n - 1) t
  | _ -> failwith "UnImpl"


(* type 'a tset_fun_input = int * ('a list) *)

let producetests test_name expect_output test_fun test_fun_input =
  match test_fun_input with
  | (n , lst) -> begin
    test_name >:: 
      (fun _ -> 
        assert_equal 
          expect_output 
          (test_fun n lst))
      end

let take_tests = "take_tests" >::: [
  "take 1 in []" >:: (fun _ -> assert_equal [] (take 1 []));
  producetests "take 3 in [1; 2]" [1; 2] take (3, [1; 2]);
  producetests "take 3 in [1; 2; 3]" [1; 2] take (3, [1; 2; 3]);
  producetests "take 3 in [1; 2; 3; 4; 5]" [1; 2] take (3, [1; 2; 4]);
]

let run_test_tt_main = take_tests 
