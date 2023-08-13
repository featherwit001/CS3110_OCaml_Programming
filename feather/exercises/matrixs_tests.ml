(* transform head element and tail elements recursively
   and concat string of head elt with string of tail elt
   with the given string_of_elt,like string_of_int
   attention: , is on the left of tail elements*)
   let interior string_of_elt h t =
      t
      |> List.map string_of_elt 
      |> List.fold_left (fun acc ele -> acc ^ "," ^ ele) (string_of_elt h)

(* the original idea to print data structure.
   first: print its external delimiter
   second: print internal elements *)
let string_of_list string_of_elt = function
      | [] -> ""
      | h :: t -> "<" ^ interior string_of_elt h t ^ ">"

(* discarded implement, the same effect with matrix_inter
   Perhaps it will help you understand how matrix_inter was implemented *)
let rec _matrix_interior h t = 
      (string_of_list string_of_int h) ^ 
      match t with 
      | [] -> ""
      | h :: t ->  ";" ^ _matrix_interior  h t  

(* to avoid such result: [<1,2>;<3.4>;],*)
let matrix_inter string_of_matrix_row h t =
      t
      |> List.map (string_of_matrix_row string_of_int)
      |> List.fold_left 
            (fun acc ele -> acc ^ ";" ^ ele) (string_of_matrix_row string_of_int h)

(* [string_of_matrix m ] is the string representation of matrix [m]
   Example: | 1 2 3 | is a matrix, the string representation is [<1,2,3>;<4,5,6>]
            | 4 5 6 |
   RI: [m] must be valid*)
let string_of_matrix string_of_matrix_row = function
      | [] -> ""
      | h :: t -> "[" ^ matrix_inter string_of_matrix_row h t ^  "]"

(* just wrapped, encapsule the implement of matrix *)
let string_of_matrix = string_of_matrix string_of_list

(* compare each row's len with the first row's len *)
let rec map_equal column_len = function
  | [] -> true
  | h :: t -> if List.length h = column_len 
              then  map_equal column_len t
              else false 

(* [] = [[]] is invalid*)
let is_valid_matrix = function
    | [] -> false
    | h :: t -> map_equal (List.length h) t

(* map a function to corresponding two elements of two lists 
   and collect the results of ecah group into a new list
   map ( + ) [1; 2] [3; 4], the result is [1 + 3; 2 + 4]  = [4; 6]*)
let rec _map2 f lst1 lst2 =
    match lst1, lst2 with
    | [], [] -> []
    | h1 :: t1, h2 :: t2 -> (f h1 h2) :: (_map2 f t1 t2)
    | _ -> failwith "Undefined"

let rec map2_aux f acc lst1 lst2 =
     match lst1, lst2 with
      | [], [] -> List.rev acc
      | h1 :: t1, h2 :: t2 -> map2_aux f ((f h1 h2 ) :: acc) t1 t2
      | _ -> failwith "Undefined"

(* tail_recursive *)
let map2 f lst1 lst2 = map2_aux f [] lst1 lst2

(* add row vector 
   Example:row_vector_add [1;2;3] [4;5;6], the result is [5;7;9]
   FI: each row has the same length *)
let row_vector_add rv1 rv2 = map2 ( + ) rv1 rv2

(* add corresponding rows of two matirx
   RI: m1 m2 have the same shape  *)
let matrix_add m1 m2 = map2 row_vector_add m1 m2

(* print array with two dimensions
   it is imperative programming. 
   attention: the return value () *)
let print_my_array array =
      let num_rows = Array.length array in
      let num_cols = Array.length array.(0) in
      for row = 0 to num_rows - 1 do
            for column = 0 to num_cols - 1 do
            Printf.printf "%d " array.(row).(column)
            done;
            Printf.printf "\n"
      done;
      ()

(* transform a two-dimemsion int array to int list list by rows
   collect each row and then merge it to result
   Example: [| 1 2 3 |]
            [| 4 5 6 |] ==> [<1,2,3>;<4,5,6>;<7,8,9>]
            [| 7 8 9 |]
   attention for ref [] and !result
   *)
let array_to_matrix array =
      let row_number = Array.length array in
      let col_number = Array.length array.(0) in
      let result  = ref[] in 
      for i = 0 to row_number - 1 do
            let row_list = ref [] in
            for j = 0 to col_number - 1 do
                  row_list := array.(i).(j) :: !row_list
            done;
            result := (List.rev !row_list) :: !result
      done;
      List.rev !result      

(* assignment the array with a row
   example | 1 2 3 |      [| 1 4 . |]
           | 4 5 6 |  ==> [| 2 5 . |]
           | . . . |      [| 3 6 . |]
*)
let rec row_to_col r row_number col_number array =
      (* print_endline  (string_of_list string_of_int r);  *)
      match r with
      | [] -> ()
      | h :: t -> begin
            array.(col_number).(row_number) <- h ; 
            row_to_col t row_number (col_number + 1) array
      end

(* assignment the [array] with matrix [m] row by row *)
let rec matrix_trans_each_row m row_number array =
      match m with
      | [] -> ()
      | h :: t -> begin
            row_to_col h row_number 0 array;
            matrix_trans_each_row t (row_number + 1)  array
      end

(* turn matrix represented by row vector 
   into matrix represented by col vector
   Example: | 1 2 3 | 
            | 5 6 7 | 
      [<1,2,3>;<5,6,7>] to [<1,5>;<2,6>;<3,7>] 
   RI: matrix [m] must be valid*)
let matrix_transpositon m =
      if is_valid_matrix m = false 
      then failwith "invalid matrix"
      else
      let row_number = List.length m in
      let col_number = List.length (List.hd m) in
      let m_array  =  Array.make_matrix col_number row_number 0 in
            matrix_trans_each_row m 0 m_array ;
            m_array |> array_to_matrix


let row_num_of_matrix m =
      List.length m
let col_number_of_matrix m =
      List.length (List.hd m)

(* one row vector multiple one col vector
   the result is a value *)
let row_plus_col row col =
      (List.map2 ( * ) row col) |> List.fold_left ( + ) 0 


(* one row  of m1 muplite all cols of m2 
   the result is a new row vector*)
let rec row_plus_cols row = function
      | [] -> []
      | h :: t -> row_plus_col row h :: (row_plus_cols row t) 

let rec row_plus_cols_aux acc row = function
      | [] -> List.rev acc
      | h :: t -> row_plus_cols_aux ((row_plus_col row h) ::acc) row t
 
(* tail_recursive  *)
let row_plus_cols row cols = row_plus_cols_aux [] row cols

(* multiple each row of m1 with each col of m2 *)
let rec matrix_multiple m1 m2 = 
      match m1 with
      | [] -> []
      | h :: t -> (row_plus_cols h m2) :: matrix_multiple t m2 

let rec matrix_multiple_aux acc m1 m2 = 
      match m1 with
      | [] -> List.rev acc
      | h :: t -> 
            let acc' = (row_plus_cols h m2) ::acc in 
            matrix_multiple_aux acc' t m2 


(* tail-recusive *)
let  matrix_multiple m1 m2 =  matrix_multiple_aux [] m1 m2
(* [ |*| m1 m2] is the result of matrix m1 plus matrix m2 
   AF: m1 is represnted by row vector, m2 is represented by col vector
   Example: |1 2 3|   plus |7 9 8 7| 
            |4 5 6|        |8 7 9 8|
                           |9 8 7 9|
  is [<1,2,3>; <4,5,6>] plus [<7,8,9>; <9,7,8>; <8,9,7>; <7,8,9>] 

     [ a1; a2] plus [b1 b2 b3 b4] = | a1*b1  a1*b2  a1*b3  a1*b4|
                                    | a2*b1  a2*b2  a2*b3  a2*b4|

  RI: [m1]'s col num is equal to [m2]'s row num                
*)
let ( |*| ) m1 m2  = 
      let _r1 = row_num_of_matrix m1 in
      let c1 = col_number_of_matrix m1 in
      let r2 = row_num_of_matrix m2 in
      let _c2 = row_num_of_matrix m2 in
      if c1 <> r2 then failwith "invaild Matrix multiple"
      else  
          let m2' = matrix_transpositon m2 in
          matrix_multiple m1 m2'



(* extreme test for large matrix  *)


let make_matrix_n_n_1 n = 
      (Array.make_matrix n n 1 ) |> array_to_matrix


let extremely_test_matrix_mutiple n = 
      (make_matrix_n_n_1 n) |*| (make_matrix_n_n_1 n) 

let your_equation x = extremely_test_matrix_mutiple x

let time_function f x =
      let start_time = Sys.time () in
      let result = f x in
      let end_time = Sys.time () in
      result, end_time -. start_time
      
let run_exetreme n =
      let x = n in
      let result, elapsed_time = time_function your_equation x in
      Printf.printf "Result: \n %s\n" (string_of_matrix result);
      Printf.printf "Elapsed time: %.6f seconds\n" elapsed_time;
      ()


(* let () =
      let args_count = Array.length Sys.argv in
      Printf.printf "命令行参数个数：%d\n" args_count;

      Printf.printf "命令行参数列表：\n";
      for i = 0 to args_count - 1 do
            Printf.printf "参数 %d: %s\n" i Sys.argv.(i)
      done *)

(*'a list = typeof [] = typeof [[]] = typeof [[[]]] = ....*)
let empty_matarix = [] 
let valid_matrix1 = [[1]]
let valid_matrix2 = [[1; 2]; [3; 4]]
let valid_matrix3 = [[1; 2; 3]; [3; 4; 5]]
let valid_matrix4 = [[3; 4; 5]; [6; 6; 8]]
let valid_matrix5 = [[4; 6; 8]; [9; 10; 13]]
let invalid_matrix1 = [[1; 2]; [3]]
let invalid_matrix2 = [[1; 2; 3]; [4; 5]]


open OUnit2
let tests_valid_matrix = "tests_valid_matrix" >::: [
    "test: []" >:: 
          (fun _ -> assert_equal false (is_valid_matrix empty_matarix));
    "test: [[1]]" >:: 
          (fun _-> assert_equal true (is_valid_matrix valid_matrix1));
    "test: [[1; 2]; [3; 4]]" >:: 
          (fun _ -> assert_equal true (is_valid_matrix valid_matrix2));
    "test: [[1; 2; 3]; [3; 4; 5]]" >:: 
          (fun _ -> assert_equal true (is_valid_matrix valid_matrix3));
    "test: [[1; 2]; [3]]" >:: 
          (fun _ -> assert_equal false (is_valid_matrix invalid_matrix1));
    "test: [[1; 2; 3]; [4; 5]]" >:: 
          (fun _ -> assert_equal false (is_valid_matrix invalid_matrix2));
]

let row_vector1 = [1; 2; 3]
let row_vector2 = [4; 5; 6]
let row_vector3 = [5; 7; 9]

let row_vector4 = [4; 5; 6; 7]

let tests_row_vector_add = "tests_row_vector_add" >::: [
      "test_rv_add_success" >:: 
            (fun _ -> 
            assert_equal 
                  row_vector3 
                  (row_vector_add row_vector1 row_vector2));
      "test_rv_add_fail" >:: 
            (fun _ -> 
            assert_raises  
                  (Failure "Undefined") 
                  (fun _ -> row_vector_add row_vector1 row_vector4));
]



let tests_matrix_add = "tests_matrix_add" >::: [
      "test_matrix_add_success" >::
      (fun _ -> 
            assert_equal 
            valid_matrix5 
            (matrix_add valid_matrix3 valid_matrix4 ));
      "test_matrix_add_fail1" >::
      (fun _ -> 
            assert_raises
            (Failure "Undefined")
            (fun _ -> matrix_add valid_matrix1 valid_matrix3));
      "test_matrix_add_fail2" >::
            (fun _ -> 
                  assert_raises
                  (Failure "Undefined")
                  (fun _ -> matrix_add invalid_matrix2 valid_matrix3));
]

let matrix_origin1 = [[1; 2]]
let matrix_transed1 = [[1]; [2]]
let matrix_origin2 = [[1; 2]; [3; 4]]
let matrix_transed2 = [[1; 3]; [2; 4]]
let matrix_origin3 = [[1; 2; 5]; [3; 4; 6]]
let matrix_transed3 = [[1; 3]; [2; 4]; [5; 6]]

let make_test_matrix_transpositon name expect input_arg =
      name >:: (fun _ -> 
            assert_equal 
            expect 
            (matrix_transpositon input_arg) 
            ~printer:string_of_matrix)

let tests_matrix_transposition = "tests_matrix_transpositon" >::: [
  (make_test_matrix_transpositon "tests_transposition_1" matrix_transed1 matrix_origin1);
  (make_test_matrix_transpositon "tests_transposition_2" matrix_transed2 matrix_origin2);
  (make_test_matrix_transpositon "tests_transposition_3" matrix_transed3 matrix_origin3);
]

let make_test_matrix_mutiple name expect input_arg1 input_arg2 =
      name >:: (fun _ -> 
            assert_equal 
            expect 
            (( |*| ) input_arg1 input_arg2) 
            ~printer:string_of_matrix)

let matrix_1 = [[1]]
let matrix_2 = [[1;2;3]; [4; 5; 6]]
let matrix_3 = [[4;7]; [5;8]; [6;9]]
let matrix_2_plus_3 = [[32; 50]; [77; 122]]
let matrix_4 = [[2;3]; [1;4]; [0;5]]
let matrix_5 = [[5;6;7]; [8;9;10]]
let matrix_4_plus_5 = [[34;39;44]; [37;42;47]; [40;45;50]]
let tests_matrix_mutiple = "tests_matrix_transpositon" >::: [
    (make_test_matrix_mutiple "tests_mutiple_1" matrix_1 matrix_1 matrix_1);
    (make_test_matrix_mutiple "tests_mutiple_2" matrix_2_plus_3 matrix_2 matrix_3);
    (make_test_matrix_mutiple "tests_mutiple_4" matrix_4_plus_5 matrix_4 matrix_5);
]

let dimensions = 10 
let tests_extreme = "tests_extreme" >::: [
      "100 dimension" >:: (fun _ -> run_exetreme dimensions);
]

let all_tests = "all_test" >::: [tests_valid_matrix; 
                                 tests_row_vector_add; 
                                 tests_matrix_add;
                                 tests_matrix_transposition;
                                 tests_matrix_mutiple;
                                 tests_extreme;] 

let _ = run_test_tt_main all_tests


