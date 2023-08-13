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

let map_trees = List.map (add_sequence MyMap.empty) [
  [(3,"3");(2,"2");(1,"1")];
  [(7,"7");(8,"8");(9,"9")];
  [(4,"4");(2,"2");(3,"3")];
  [(6,"6");(8,"8");(7,"7")];
]
let str_tree3 = "[(1,\"1\",1); (2,\"2\",2); (3,\"3\",1)]"  
let str_tree4 = "[(7,\"7\",1); (8,\"8\",2); (9,\"9\",1)]"
let str_tree5 = "[(2,\"2\",1); (3,\"3\",2); (4,\"4\",1)]"
let str_tree6 = "[(6,\"6\",1); (7,\"7\",2); (8,\"8\",1)]"

let str_trees = [str_tree3;str_tree4;str_tree5;str_tree5]

let string_of_map_h m = string_of_binding_h (MyMap.binding_h m )

let tests_MyMap_easy = "tests_MyMap_easy" >::: [
  "is_empty" >:: (fun _ -> assert_equal true (MyMap.is_empty map_tree1));
  "[]" >:: (fun _ -> 
      assert_string_equal str_tree1 (string_of_map_h map_tree1));
  "[(1,\"1\",1)]" >:: (fun _ -> 
      assert_string_equal str_tree2 (string_of_map_h map_tree2));
  "add 3 2 1" >:: (fun _ -> 
      assert_string_equal str_tree3 (string_of_map_h map_tree3));
  "add 7 8 9" >:: (fun _ -> 
      assert_string_equal str_tree4 (string_of_map_h map_tree4));
  "add 4 2 3" >:: (fun _ -> 
      assert_string_equal str_tree5 (string_of_map_h map_tree5));
  "add 6 8 7" >:: (fun _ -> 
      assert_string_equal str_tree6 (string_of_map_h map_tree6));  
]

(* let make_tests_for_MyMap name input *)

let all_tests_for_bst = "all_test_for_bst" >::: 
                                (tests_is_bst @ tests_is_bst )

let all_tests = "all_test" >::: [tests_MyMap_easy;
                                 tests_precaution_MyMap;
                                ]
                                 

let _ = run_test_tt_main all_tests