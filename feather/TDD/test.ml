open OUnit2
open Nextday
 
let tests_for_workday = "test suite for nextday" >::: [
  "Tue after Mon" >:: (fun _ -> assert_equal Tuesday (nextday Monday) );
  "Wed after Tue" >:: (fun _ -> assert_equal Wednesday (nextday Tuesday));
  "Fri after Thu" >:: (fun _ -> assert_equal Friday (nextday Thursday ));
]

let tests_for_weekend = "test suite for weekend nextday" >::: [
  "Sun after Saturday" >:: (fun _ -> assert_equal Sunday (nextday Saturday));
]


let _ = run_test_tt_main  ("All Tests" >::: [tests_for_workday; tests_for_weekend])