open OUnit2
open Maps_review

let pp_string s = "\"" ^ s ^ "\""

let pp_list pp_elt lst =
  (* interior elt ,namely [ "...." ] *)
  let pp_elts lst = 
    let rec loop n acc = function
      | [] -> acc (* end of list return acc as result*)
      | [h] -> acc ^ pp_elt h (* last one elt, concat without ";" *)
      | h1 :: (_h2 :: _t as t') ->
      if n = 100 then acc ^ "..." (* stop printing too large list *)
      else let acc' = acc ^ pp_elt h1 ^ "; " in (* normal case, concat next elt with ";" *)
      loop (n + 1) acc' t'
    in 
      loop 0 "" lst (*pp_lets lst = loop 0 "" lst*)
  in "[" ^ pp_elts lst ^ "]" (*pp_list = "[" ^ pp_lets lst ^ "]"*)
  
let pp_pair pp1 pp2 (f, s) =
  "(" ^ pp1 f ^", "^ pp2 s ^ ")"

let pp_binding_list = pp_list (pp_pair string_of_int pp_string)   

(* compare depends on OCaml runtime enviroment,
   it could compare accoding to the type of values, 
   even if a composite type*)
let cmp_lists_like_set lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length uniq1 = List.length lst1
  &&
  List.length uniq2 = List.length lst2
  &&
  uniq1 = uniq2

let lst1 = [(3, "fun")]
let lst2 = [(3, "fun");  (2, "OO")]
let lst2' = [(3, "OO");  (2, "OO")]

let lst3 = [(3, "fun"); (3, "OO"); (2, "OO")]

open ListMap

let listmap_binding_test name expect input =
  name >:: (fun _ -> 
    assert_equal expect (bindings input) 
    ~printer:pp_binding_list
    ~cmp: cmp_lists_like_set)

(* these auxiliary functions is so helpful
   implements is really simple,
   and the module function design provide a quite easy way to 
   batch input and type convert 
   which bypasses type barrier between concrete type and module type*)

let tests_for_Listmap = "tests_for_ListMap" >::: [
    listmap_binding_test "empty" [] empty;
    listmap_binding_test "1 elt" lst1 (of_list lst1);
    listmap_binding_test "2 elt" lst2 (of_list lst2);
    listmap_binding_test "duplicates" lst2 (of_list lst3);
]

open ArrayMap
let arraymap_binding_test name expect input =
  name >:: (fun _ -> 
    assert_equal expect (bindings input) 
    ~printer:pp_binding_list
    ~cmp: cmp_lists_like_set)

let tests_for_ArrayMap = "tests_for_ArrayMap" >::: [
  arraymap_binding_test "empty" [] (create 10);
  arraymap_binding_test "1 elt" lst1 (of_list 10 lst1);
  arraymap_binding_test "2 elt" lst2 (of_list 10 lst2);
  arraymap_binding_test "duplicates" lst2' (of_list 10 lst3);
]

open HashMap

let hashmap_binding_test name expect input =
  name >:: (fun _ -> 
    assert_equal expect (bindings input)
    ~printer:pp_binding_list
    ~cmp: cmp_lists_like_set)

let hash = Hashtbl.hash 
let tests_for_HashMap = "tests_for_ArrayMap" >::: [
  hashmap_binding_test "empty" [] (create hash 1);
  hashmap_binding_test "1 elt" lst1 (of_list hash lst1);
  hashmap_binding_test "2 elt" lst2 (of_list hash lst2);
  hashmap_binding_test "duplicates" lst2' (of_list hash lst3);
]

let all_tests = "all_tests" >::: [tests_for_Listmap;
                                  tests_for_ArrayMap;
                                  tests_for_HashMap
                                  ]

                                  
let _ = run_test_tt_main all_tests

