let leap_year y = 
  y mod 4 = 0
  &&
  (y mod 100 <> 0 || y mod 400 = 0)

open OUnit2

let leap_year_test (n, y, b) =
  n >:: (fun _ -> assert_equal b (leap_year y))

let tests_leap_year = List.map leap_year_test [
  "non leap year", 2010, false;
  "non-centennial", 2020, true;
  "quadracentennial", 2000, true;
]

let suite = "leap_year" >::: tests_leap_year

let _ = run_test_tt_main suite
