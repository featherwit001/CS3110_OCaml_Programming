open OUnit2
open Bsts

let v1 = (1, "1")
let v2 = (2, "2")
let v3 = (3, "3")
let v4 = (4, "4")
let v5 = (5, "5")
let v6 = (6, "6")
let v7 = (7, "7")
let v8 = (8, "8")
let v9 = (9, "9")
let v10 = (10, "10")
let v20 = (20, "20")


let bst1 = Node (v5, 
              Node (v3, 
                Node(v1, Leaf, Leaf),
                Node(v4, Leaf, Leaf)
              ),
              Node (v8, 
                Node(v7, Leaf, Leaf), 
                Node(v9, Leaf, Leaf)
              )
            )
let bst2 = Node (v5,
            Node(v4, 
              Node(v3, 
                Node(v2, 
                  Node(v1, Leaf, Leaf),
                  Leaf
                ), 
                Leaf
              ), 
              Leaf
            ),
            Leaf
           )
let bst3 = Node (v1, Leaf,
            Node(v2, Leaf,
              Node(v3, Leaf,
                Node(v4, Leaf,
                  Node(v5, Leaf,Leaf)))))
let bst4 = Node (v10, 
            Node(v5, 
              Node(v2, Leaf, Leaf),
              Node(v7, Leaf, Leaf)
            ),
            Node(v20, Leaf,Leaf)
           )
let notbst1 = Node (v5, 
                Node (v3, 
                  Node(v1, Leaf, Leaf),
                  Node(v7, Leaf, Leaf)
                ),
                Node (v8, 
                  Node(v4, Leaf, Leaf), 
                  Node(v9, Leaf, Leaf)
                )
              )
let notbst2 = Node(v10, 
                Node(v5, 
                  Node(v7, Leaf, Leaf),
                  Node(v2, Leaf, Leaf)
                ),
                Node(v20, Leaf, Leaf)
              )
let notbst3 = Node (v1, Leaf,
                Node(v2, Leaf,
                  Node(v7, Leaf,
                    Node(v4, Leaf,
                      Node(v5, Leaf,Leaf)))))
let notbst4 = Node (v5,
                Node(v4, 
                  Node(v3, 
                    Node(v2, 
                      Node(v20, Leaf, Leaf),
                      Leaf
                    ), 
                    Leaf
                  ), 
                  Leaf
                ),
                Leaf
                ) 

let make_test_is_bst (name, expect, input) =
  name >:: (fun _ -> assert_equal expect (is_bst compare_int input))

let bst_easy = Node(v2, 
                Node(v1, Leaf, Leaf),
                Node(v3, Leaf, Leaf))
let tests_is_bst = List.map make_test_is_bst [
  "test_easy", true, bst_easy;
  "normal bst", true, bst1;
  "left bst", true, bst2;
  "right bst", true, bst3;
  "unbalance bst", true, bst4;
  "not bst1", false, notbst1;
  "not bst2", false, notbst2;
  "not bst3", false, notbst3;
  "not bst4", false, notbst4;
]

let make_test_find_bst (name, expect, inputarg1, inputarg2) =
  name >:: (fun _ -> 
     assert_equal 
     expect
     (find_bst compare_int inputarg1 inputarg2))

let tests_find_bst = List.map make_test_find_bst [
  "find 1", Some "1", 1, bst1;
  "find 3", Some "3", 3, bst1;
  "find 9", Some "9", 2, bst1;
  "find 8", Some "8", 5, bst1;
  "find 4", Some "4", 4, bst1;
  "find 10", None   , 10, bst1;
  "find 1", Some "1", 10, bst2;
  "find 1", Some "5", 5, bst3;
]


let str_exp1 = "[(1,\"1\"); (2,\"2\"); (3,\"3\")]"
let binding_input = [(1,"1");(2,"2");(3,"3")]
let str_exp2 = "[(1,\"1\",1); (2,\"2\",2); (3,\"3\",1)]"
let binding_h_input = [(1,"1",1);(2,"2",2);(3,"3",1)]

let assert_string_equal expected actual =
  assert_equal ~printer:(fun x -> x) expected actual

let tests_precaution_MyMap = "tests_precaution_MyMap" >:::[
  "string_of_binding" >:: (fun _ -> 
    let str_output = string_of_binding binding_input in
    assert_string_equal str_exp1 str_output);
  "string_of_binding_h" >:: (fun _ ->
    let str_output = string_of_binding_h binding_h_input in
    assert_string_equal str_exp2 str_output);
]

module Order_int_string = struct
  type t = int
  let compare key1 key2 =
    key1 - key2
end

module MyMap = Make(Order_int_string)

let map_tree1 = 
  let open MyMap in
  empty
let str_tree1 = "[]"

let map_tree2 = 
  let open MyMap in
  empty
  |> add 1 "1"
let str_tree2 = "[(1,\"1\",1)]"
let str_tree2_modified = "[(1,\"1\")]"
let rec add_sequence acc = function
  | [] -> acc
  | (k, d) :: t -> 
    let open MyMap in
    let acc' = add k d acc in
    add_sequence acc' t
let map_tree3 = add_sequence MyMap.empty [(3,"3");(2,"2");(1,"1")]
let map_tree4 =  add_sequence MyMap.empty [(7,"7");(8,"8");(9,"9")]
let map_tree5 = add_sequence MyMap.empty [(4,"4");(2,"2");(3,"3")]
let map_tree6 = add_sequence MyMap.empty [(6,"6");(8,"8");(7,"7")]
let str_tree3 = "[(1,\"1\",1); (2,\"2\",2); (3,\"3\",1)]"  
let str_tree4 = "[(7,\"7\",1); (8,\"8\",2); (9,\"9\",1)]"
let str_tree5 = "[(2,\"2\",1); (3,\"3\",2); (4,\"4\",1)]"
let str_tree6 = "[(6,\"6\",1); (7,\"7\",2); (8,\"8\",1)]"

let map_tree7 = let open MyMap in
    let n7 = Node {l = Empty; k = 7; d = "7"; r = Empty; h = 1} in
    let n9 = Node {l = n7; k = 9; d = "9"; r = Empty; h = 2} in 
    let n11 = Node {l = Empty; k = 11; d = "11"; r = Empty; h = 1} in
    Node {l = n9; k = 10; d = "10"; r = n11; h = 3}


let str_tree7 = "[(7,\"7\",1); (9,\"9\",2); (10,\"10\",3); (11,\"11\",1)]"
let map_tree7_rm11 = MyMap.remove 11 map_tree7
let str_tree7_rm11 = "[(7,\"7\",1); (9,\"9\",2); (10,\"10\",1)]"
let str_tree7_rm10 = "[(7,\"7\",1); (9,\"9\",2); (11,\"11\",1)]"

let map_tree8 = let open MyMap in
  let n7 = Node {l = Empty; k = 7; d = "7"; r = Empty; h = 1} in
  let n9 = Node {l = n7; k = 9; d = "9"; r = Empty; h = 2} in 
  let n18 = Node {l = Empty; k = 18; d = "18"; r = Empty; h = 1} in
  let n20 = Node {l = n18; k = 20; d = "20"; r = Empty; h = 2} in 
  let n14 = Node {l = Empty; k = 14; d = "14"; r = Empty; h = 1} in
  let n15 = Node {l = n14 ; k = 15; d = "15"; r = n20; h = 3} in
  Node {l = n9; k = 10; d = "10"; r = n15; h = 4}

let not_AVL_map1 = let open MyMap in
  let n2 = Node {l = Empty; k = 2; d = "2"; r = Empty; h = 1} in
  Node {l = n2; k = 1; d = "1"; r = Empty; h = 2}

let string_of_map_h m = string_of_binding_h (MyMap.binding_h m )
let string_of_map m = string_of_binding (MyMap.binding m)
let tests_MyMap_easy = "tests_MyMap_easy" >::: [
  "is_empty" >:: (fun _ -> assert_equal true (MyMap.is_empty map_tree1));
  "[]" >:: (fun _ -> 
      assert_string_equal str_tree1 (string_of_map_h map_tree1));
  "[(1,\"1\",1)]" >:: (fun _ -> 
      assert_string_equal str_tree2 (string_of_map_h map_tree2));
  "[(1,\"1\")]" >:: (fun _ -> 
      assert_string_equal str_tree2_modified (string_of_map map_tree2));
  "add 3 2 1" >:: (fun _ -> 
      assert_string_equal str_tree3 (string_of_map_h map_tree3));
  "add 7 8 9" >:: (fun _ -> 
      assert_string_equal str_tree4 (string_of_map_h map_tree4));
  "add 4 2 3" >:: (fun _ -> 
      assert_string_equal str_tree5 (string_of_map_h map_tree5));
  "add 6 8 7" >:: (fun _ -> 
      assert_string_equal str_tree6 (string_of_map_h map_tree6));
  "add overwrite" >:: (fun _ -> 
      let str_tree3_modified = "[(1,\"1\",1); (2,\"2\",2); (3,\"33\",1)]" in
      let map_tree3_modified = MyMap.add 3 "33" map_tree3 in
      assert_string_equal str_tree3_modified (string_of_map_h map_tree3_modified));
  "tree 7" >:: (fun _ -> 
      assert_string_equal str_tree7 (string_of_map_h map_tree7));
  "tree 7 is_bal" >:: (fun _ -> 
      assert_equal true (MyMap.is_bal map_tree7));
  "tree 7find 7" >:: (fun _ -> assert_equal "7" (MyMap.find 7 map_tree7)); 
  "tree 7 find 11" >:: (fun _ -> assert_equal "11" (MyMap.find 11 map_tree7)); 
  "tree 7 mem 7" >:: (fun _ -> assert_equal true (MyMap.mem 7 map_tree7));
  "tree 7 mem 11" >:: (fun _ -> assert_equal true (MyMap.mem 7 map_tree7));
  "tree 7 remove 11" >:: (fun _ -> 
      assert_string_equal str_tree7_rm11 (string_of_map_h map_tree7_rm11));
  "tree 7 mem 11 after rm11" >:: (fun _ -> assert_equal false (MyMap.mem 11 map_tree7_rm11));
  "tree 7 find 11 after rm11" >:: (fun _ ->
      assert_raises Not_found (fun _ -> MyMap.find 11 map_tree7_rm11));
  "tree7 rm 10" >:: (fun _ ->
    let map_tree7_rm10 = MyMap.remove 10 map_tree7 in
    assert_string_equal str_tree7_rm10 (string_of_map_h map_tree7_rm10));

]

let tests_MyMap_normal = "tests_MyMap_normal"  >:::[
  "tree 8 is_bal" >::  (fun _ -> 
    assert_equal true (MyMap.is_bal map_tree8);
    (* assert_equal "" (string_of_map_h map_tree8) ~printer:(fun x -> x) *)
    );
  "tree 8 is_ordered" >::  (fun _ -> 
    assert_equal true (MyMap.is_ordered map_tree8));
  "tree 8 is_AVL" >:: (fun _ -> 
    assert_equal true (MyMap.is_AVL map_tree8));
  "not_AVL_map1 is_bal" >:: (fun _ -> 
    assert_equal true (MyMap.is_bal not_AVL_map1));
  "not_AVL_map1 not ordered" >:: (fun _ -> 
      assert_equal false (MyMap.is_ordered not_AVL_map1));
  "not_AVL_map1 not AVL" >:: (fun _ -> 
    assert_equal false (MyMap.is_AVL not_AVL_map1));
  "tree 8 rm 7" >:: (fun _ ->
    assert_equal true (MyMap.is_AVL (MyMap.remove 7 map_tree8)));
  "tree 8 rm 10" >:: (fun _ ->
    assert_equal true (MyMap.is_AVL (MyMap.remove 10 map_tree8)));

]
(* let make_tests_for_MyMap name input *)

let all_tests_for_bst = "all_test_for_bst" >::: 
                                (tests_is_bst @ tests_is_bst )

let all_tests = "all_test" >::: [
                                 (* tests_precaution_MyMap; *)
                                 (* tests_MyMap_easy; *)
                                 tests_MyMap_normal;
                                 (* all_tests_for_bst; *)
                                 ]  
                                 

let _ = run_test_tt_main all_tests

(* ------------------------------------------- *)
let gen_int_string_tuple = QCheck.Gen.(tup2 int string)
let gen_custom = QCheck.Gen.(list_size (int_range 1000 1000) gen_int_string_tuple)

let arb = QCheck.make gen_custom
let is_AVLq input=
  MyMap.is_AVL (add_sequence MyMap.empty input)

let qtest = QCheck.Test.make ~name:"AVL add" ~count:1000 arb is_AVLq

let _ = QCheck_runner.run_tests  ~verbose:true [qtest]