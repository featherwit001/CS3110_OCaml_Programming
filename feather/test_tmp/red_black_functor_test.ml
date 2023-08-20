open OUnit2
open Red_black_functor

module RBT = MakeRBTree(IntCmp)

(* Tests *)
let empty  = RBT.empty
let insert = RBT.insert
let member = RBT.member
let delete = RBT.delete
let bindings = RBT.bindings
let of_list = RBT.of_list
let is_RBTree = RBT.is_RBTree

let pp_string s = "\"" ^ s ^ "\""

let pp_list pp_elt lst = 
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 ::( _h2 :: _t' as t) ->
        if  n >= 100 then  acc ^ "......"
        else loop (n + 1) (acc ^ pp_elt h1 ^ "; " ) t
      in
        loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"
let pp_int = string_of_int
let pp_bindings_RBt lst = pp_list pp_int lst 

let cmp_lists_like_set lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length uniq1 = List.length lst1
  &&
  List.length uniq2 = List.length lst2
  &&
  uniq1 = uniq2

let list1 = [5; 10; 2; 12; 6]
let list2 = [5; 2; 12; 6]
let list3 = [5; 2; 12; 10]

let tree1 = of_list list1

let make_member_test name expect inputarg1 inputarg2 = 
  name >:: (fun _ -> assert_equal expect (member inputarg1 inputarg2))  

let make_delete_test name expect inputarg1 inputarg2 =
  name >:: (fun _ -> 
    let t' = delete inputarg1 inputarg2 in
    assert_equal expect (bindings t')
    ~printer:pp_bindings_RBt
    ~cmp: cmp_lists_like_set)  

let tests_easy = "tests_easy" >::: [
  make_member_test "5" true tree1 5;
  make_member_test "10" true tree1 10;
  make_member_test "2" true tree1 2;
  make_member_test "12" true tree1 12;
  make_member_test "6" true tree1 6;
  make_member_test "15" false tree1 15;
  make_delete_test "del 10" list2 tree1 10;
  make_delete_test "del 6" list3 tree1 6;
]


open QCheck

let random_add_and_check 
  ?(max_len=40) 
  ?(min_value=(-20)) 
  ?(max_value=20)
  ()
  =
  let seed = Random.State.make_self_init () in
  let gen_list_len = Gen.(int_range 0 max_len) in
  let gen_int_in_boundry = Gen.(int_range min_value max_value) in
  let gen_add_list = Gen.(list_size gen_list_len gen_int_in_boundry) in
  let list1 = Gen.(generate1 ~rand:seed gen_add_list) in
  let tree1 = of_list list1 in
  is_RBTree tree1

let random_add_test = "random add" >::: [
  "random add and check" >:: (fun _ -> assert_equal true (random_add_and_check() ))
]
let all_tests = "all_tests" >::: [tests_easy;
                                  random_add_test]

let _ = run_test_tt_main all_tests

let max_len = 20000
let max_value = max_len / 2
let min_value = - max_value
let gen_list_len = Gen.(int_range 0 max_len) 
let gen_int_in_boundry = Gen.(int_range min_value max_value) 
let gen_list_to_add = Gen.(list_size gen_list_len gen_int_in_boundry) 
let gen_list_to_del = Gen.(list_size gen_list_len gen_int_in_boundry) 
let gen_tup = Gen.(tup2 gen_list_to_add gen_list_to_del)

(*Test: add any elt, it is always a RB Tree *)
let test_is_RBT lst =
  is_RBTree (of_list lst)


(* Test: add any elt and remove any elt. it is always a RB Tree
   and all the elta that should be removed is not exist in this tree
   and all the elts that aren't in the remove list should still exsit.*)
let test_add_and_del (lst1, lst2) =
  let tree1 = of_list lst1 in
    let rec del_seq tree = function
      | [] -> tree
      | h :: t -> let tree' = delete tree h in
      del_seq tree' t
    in 
  let module IntOrder = struct
        type t = int
        let compare t1 t2 =
          t1 - t2
      end  
  in  
    let module Myset = Set.Make(IntOrder) in
      let setadd = Myset.of_list lst1 in 
      let setdel = Myset.of_list lst2 in
      let removed_elts = Myset.inter setadd setdel in
      let still_exist = Myset.diff setadd removed_elts in 
      let tree1' = del_seq tree1 lst2 in 
  Myset.for_all (fun x -> member tree1' x = false) removed_elts
  &&
  Myset.for_all (fun x -> member tree1' x) still_exist
  &&
  is_RBTree tree1'

let arb_gen_list = QCheck.make gen_list_to_add ~print:pp_bindings_RBt
let qtest_is_RBTree = QCheck.Test.make ~name:"random add" ~count:1000 
                            arb_gen_list test_is_RBT

let arb_tup =  QCheck.make gen_tup 
let qtest_add_and_del = QCheck.Test.make ~name:"add_and_del" ~count:1000 
                        arb_tup test_add_and_del

let _ = QCheck_runner.run_tests ~verbose: true [qtest_is_RBTree;qtest_add_and_del]

