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

let rec from i j lst = if i <= j then from i (j - 1) (j :: lst) else lst
let ( -- ) i j = from i j []

let rec remove_sequence m = function
  | [] -> m
  | h :: t -> (remove_sequence (MyMap.remove h m) t)

let tests_rm_seq_aux i j m n = 
  let sequences = i -- j in
  let add_sequences = List.map (fun x -> (x , string_of_int x)) sequences in
  let map_large = add_sequence MyMap.empty add_sequences in

  let remove_sequences = m -- n in
  let map_rmed = remove_sequence map_large remove_sequences in
  MyMap.is_AVL map_rmed


let seed_array = [|123; 456; 789|] 
let seed_custom = Random.State.make seed_array
let rm_random_list i j m n =
  let gen_random_int_list = QCheck.Gen.(list_size (int_range i j) (int_range m n)) in 
  QCheck.Gen.(generate1 ~rand:seed_custom gen_random_int_list)

let rm_random_seq1 = 
  [68; 46; 43; 9; 48; 28; 80; 32; 47; 93; 98; 61; 
   98; 46; 65; 71; 48; 69; 62; 43;88; 30; 43; 95; 
   10; 60; 18; 38; 92; 39; 14; 28; 66; 93; 7; 93; 97; 5; 0]

let tests_rm_random_seq_aux i j rm_seq = 
    let sequences = i -- j in
    let add_sequences = List.map (fun x -> (x , string_of_int x)) sequences in
    let map_large = add_sequence MyMap.empty add_sequences in
    if MyMap.is_AVL map_large = false then false
    else 
    let remove_sequences = rm_seq in
    let map_rmed = remove_sequence map_large remove_sequences in
    MyMap.is_AVL map_rmed

let tests_MyMap_remove_seq = "tests_MyMap_remove_seq" >::: [
  "add 0 - 10 rm 5 - 10" >:: (fun _ ->
    assert_equal true (tests_rm_seq_aux 0 10 5 10));
  "add 0 - 10 rm -5 - 5" >:: (fun _ ->
    assert_equal true (tests_rm_seq_aux 0 10 (-5) 5));
  "add 0 - 100 rm -5 - 50" >:: (fun _ ->
      assert_equal true (tests_rm_seq_aux 0 100 (-5) 50));
  "add -100 - 100 rm -50 - 50" >:: (fun _ ->
      assert_equal true (tests_rm_seq_aux (-100) 100 (-50) 50));
  "add 0-100 rm random from _0-100" >::(fun _ ->
    assert_equal true (tests_rm_random_seq_aux (-100) 100 rm_random_seq1));
]

let random_add_remove_aux ?(bound_up_down=40) ?(min=(-20)) ?(max=20) () =
  let seed_array = [|323892493|] in
  let seed_custom = Random.State.make seed_array in
  let gen_random_int_list = 
    QCheck.Gen.(list_size (int_range 0 bound_up_down) (int_range min max)) in 
  let list1 = QCheck.Gen.(generate1 ~rand:seed_custom gen_random_int_list) in
  let list2 = QCheck.Gen.(generate1 ~rand:seed_custom gen_random_int_list) in
  let map = add_sequence MyMap.empty (List.map (fun x -> (x, string_of_int x)) list1) in
  let rec remove_step_by_step map = function
    | [] -> (map, MyMap.Empty, list1, list2, 0)
    | h :: t -> begin 
      let origin_map = (MyMap.clone map) in
      let map' = (MyMap.remove h map) in
      if (MyMap.is_AVL map') = false then 
        let _ = print_endline ("failwith remove " ^ (string_of_int h)) in
      (map', origin_map, list1, list2, h)
      else remove_step_by_step map' t
    end
  in 
    remove_step_by_step map list2


let tests_random_add_remove = "tests_random_add_remove" >::: [
  "random_add_remove" >:: (fun _ -> (
    let m1, _m2, addlst, rmlst, h = random_add_remove_aux () in
    print_endline (string_of_list string_of_int addlst);
    print_endline (string_of_list string_of_int rmlst);
    print_endline ("when add h = " ^ (string_of_int h));
    assert_equal true (MyMap.is_AVL m1)
  ));
]


let all_tests_for_bst = "all_test_for_bst" >::: 
                                (tests_is_bst @ tests_is_bst )

let all_tests = "all_test" >::: [
                                 tests_precaution_MyMap;
                                 tests_MyMap_easy;
                                 tests_MyMap_normal;
                                 tests_MyMap_remove_seq;
                                 tests_random_add_remove;
                                 all_tests_for_bst;
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

(* ------------------------------------------------ *)
(* success *)
let seq1000 = (-10000) -- 10000

let addseq1000 = List.map (fun x -> (x, string_of_int x)) seq1000

let map100_random_rm_is_AVL rm_ramdom_seq =
  let map1000 = add_sequence MyMap.empty addseq1000 in
  MyMap.is_AVL (remove_sequence map1000 rm_ramdom_seq)

let gen_rm_seq = QCheck.Gen.(list_size (int_range 0 20000) (int_range (-10000) 10000))

let arb_rm_seq = QCheck.make gen_rm_seq

let qtests_map1000_random_rm = QCheck.Test.make ~name:"map1000 random rm" ~count:1000 
                               arb_rm_seq map100_random_rm_is_AVL

let _ = QCheck_runner.run_tests  ~verbose:true [qtests_map1000_random_rm]

(* it takes me a lot of time to debug 
   and find out that removing_min_binding forgets to balance.
   but now all of tests are successful!*)