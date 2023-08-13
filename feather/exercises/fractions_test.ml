open OUnit
open Fractions.TupleFraction
 
(* primitive type is different from module type t
   so assert_equal need some method to compare expect and output *)
let make_tests_frac_make name expect input_arg1 input_arg2 =
  let (p, q) = expect in (name >:: (fun _ -> 
            assert_equal
            (make_directly p q) 
            (make input_arg1 input_arg2) 
            ~printer:to_string))
let tests_frac_make = "tests_frac_make" >::: [
  "div 0" >:: (fun _ -> (assert_raises (Failure "div zero") (fun _ -> make 15 0)));
  make_tests_frac_make " 5 /  15" ( 1, 3)   5   15;
  make_tests_frac_make "-5 /  15" (-1, 3) (-5)  15;
  make_tests_frac_make " 5 / -15" (-1, 3)   5 (-15);
  make_tests_frac_make "42 /  -7" (-6, 1)  42 ( -7);
  make_tests_frac_make "100 / 25" ( 4, 1) 100  25;
  make_tests_frac_make "0 / 1"   ( 0, 1)   0   32;
]

let make_tests_frac_add name expect input_arg1 input_arg2 =
  name >:: (fun _ -> 
            assert_equal
            expect
            (add input_arg1 input_arg2) 
            ~printer:to_string)
let frac1 = make 1 3
let frac2 = make 2 3
let frac3 = make 1 (-3)
let frac4 = make 0 1
let frac5 = make (-34) (53)
let frac6 = make (73) (93)
let frac7 = make 707 4929

let tests_frac_add = "tests_frac_add" >::: [
  make_tests_frac_add "1/3 + 1/3" frac2 frac1 frac1;
  make_tests_frac_add "1/3 - 1/3" frac4 frac1 frac3;
  make_tests_frac_add "-34/53 - 73/93" frac7 frac5 frac6;
]

let make_tests_frac_mul name expect input_arg1 input_arg2 =
  name >:: (fun _ -> 
            assert_equal
            expect
            (mul input_arg1 input_arg2) 
            ~printer:to_string)
let frac_mul1 = make 1 1
let frac_mul2 = make 0 32
let frac_mul3 = make 0 1
let frac_mul4 = make 4 5
let frac_mul5 = make 25 24
let frac_mul6 = make 5 6
let tests_frac_mul = "tests_frac_add" >::: [
  make_tests_frac_mul "1 * 1" frac_mul1 frac_mul1 frac_mul1;
  make_tests_frac_mul "1 * 0" frac_mul3 frac_mul2 frac_mul3;
  make_tests_frac_mul "4/5 * 25/24" frac_mul6 frac_mul4 frac_mul5;
]

let tests_easy = "tests_easy" >::: [
  "numerator" >:: (fun _ -> assert_equal 4 (numerator frac_mul4) ~printer:string_of_int);
  "denominator" >:: (fun _ -> assert_equal 5 (denominator frac_mul4)~printer:string_of_int);
  "to_float" >:: (fun _ -> assert_equal 0.8 (to_float frac_mul4)~printer:string_of_float);
  "to_string" >:: (fun _ -> assert_equal"<4 / 5>" (to_string frac_mul4));
]

let all_tests = "all_tests" >::: [tests_frac_make; 
                                  tests_frac_add;
                                  tests_frac_mul;
                                  tests_easy]

let _ = run_test_tt_main all_tests