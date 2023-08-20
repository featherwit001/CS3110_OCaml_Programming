open OUnit2
open Main

let make_i n i s =
  n >:: (fun _ -> assert_equal (string_of_int i) (interp s)~printer:(fun x -> x))

let make_f n f s =
  n >:: (fun _ -> assert_equal (string_of_float f) (interp s)~printer:(fun x -> x))
let tests = [
  make_i "int" 22 "22";
  make_i "int" (-22) "-22";
  make_i "add" 22 "11+11";
  make_i "mult" 22 "2*11";
  make_i "mult of mult" 40 "2*2*10";
  make_i "mult on right of add" 22 "2+2*10";
  make_i "mult on left of add" 14 "2*2+10";
  make_i "nested add" 22 "(10 + 1 ) + ( 5 + 6)";
  make_i "nested add and mult" 121 "(10 + 1 ) * ( 5 + 6)";
  make_i "minus" 5 "10-5";
  make_i "minus and neg" 15 "10--5";
  make_i "div" 2 "10 / 5";
  make_i "complex" 0 "1+(10 - 3)*2/7-(5*4/10+2-1)";
  make_f "float" 22.0 "22.";
  make_f "float" 22.0 "22.0";
  make_f "float neg" (-22.0) "-22.0";
  make_f "add" 22.0 "11.0 +11.";
  make_f "mult" 22.0 "11.0 * 2";
  make_f "minus" 10.0 "15 - 5.0";
  make_f "float minus neg" 22.0 "20.--2.";
  make_f "div"   2.5 "5 / 2";
  make_f "nested" 6.25 "10*(2/(5+3)*(7-4.5))";
]

let _ = run_test_tt_main ("suite" >::: tests)