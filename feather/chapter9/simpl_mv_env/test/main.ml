open OUnit2
open Interp
open Ast
open Main

let make_i_s n i s =
  n >:: (fun _ -> assert_equal (VInt i) (interp_small s))
let make_b_s n b s =
  n >:: (fun _ -> assert_equal (VBool b) (interp_small s))
let make_err_s n s' s =
  n >:: (fun _ -> assert_raises (Failure s') (fun _ -> (interp_small s)))



(** [make_i n i s] makes an OUnit test named [n] that expects
    [s] to evalute to [Int i]. *)
let make_i n i s =
  n >:: (fun _ -> assert_equal (VInt i) (interp s))

(** [make_b n b s] makes an OUnit test named [n] that expects
[s] to evalute to [Bool i]. *)
let make_b n b s =
  n >:: (fun _ -> assert_equal (VBool b) (interp s))
    
(** [make_errs n s] makes an OUnit test named [n] that expects
    [s] to fail type checking with error string [s']. *)
let make_err n s' s =
   n >:: (fun _ -> assert_raises (Failure s') (fun _ -> (interp s)))

let tests = [
  make_i "int" 22 "22";
  make_i "add" 22 "11+11";
  make_i "adds" 33 "11 + 11 + 11";

  make_i "mult" 22 "2*11";
  make_i "mult of mult" 40 "2*2*10";
  make_i "mult on right of add" 22 "2+2*10";
  make_i "mult on left of add" 14 "2*2+10";
  
  make_i "nested add" 22 "(10 + 1 ) + ( 5 + 6)";
  make_i "nested add and mult" 121 "(10 + 1 ) * ( 5 + 6)";
  make_i "nested parenthese" 124 "2*(5*(4 + (3* 2) +1) + 7)";
  
  make_b "true" true "true";
  make_b "false" false "false";
  make_b "leq" true "10<=22";
  make_b "leq" false "22<=10";

  make_i "let" 22 "let x = 22 in x";
  make_i "letlet x" 22 "let x = 11 in let x = 22 in x";
  make_i "letlet xy" 22 "let x = 11 in let y = x in y + y";
  make_i "letetlet xyz" 4 "let x = 1 in let y = 0 in let z = 3 in x + y + z" ;
  make_i "if f" 0 "if false then 22 else 0";
  make_i "if f" 22 "if true then 22 else 0";
  make_i "if2" 10 "if 4<=2 then 22 else 10";
  make_i "if3" 22 "if 1+2<=3+4 then 22 else 0";
  make_i "ifif1" 22 "if 1+2 <= 4 then if 9<=1+2 then 0 else 22 else 9";
  make_i "ifif1" 22 "if 3+2 <= 4 then 9 else if 9<=1+2 then 0 else 22";
  make_i "iflet" 22 "if 1+2 <=3*4 then let x = 22 in x else 0" ;
  make_i "letif" 22 "let x = 1+2 <=3*4 in if x then 22 else 0";
  make_i "ifletif" 10 "if true 
                       then let x = 3 in if x <= 4 then 10 else 22
                       else let x = 5 in if x <= 4 then 12 else 24";
  make_i "letiflet" 5 "let x = 3 in 
                            if x <= 4 then let y = 2 in x + y
                                      else let z = 5 in x + z";
  make_err "ty plus" bop_err "1+true";
  make_err "ty mult" bop_err "1 * false";
  make_err "ty leq" bop_err "true <= 1";
  make_err "if_guard" if_guard_err "if 1 then 2 else 3";
  make_err "unbound" (unbound_var_err ^" x") "x";
]

let code_part = function
  |  (Closure (x, e, _)) -> Fun (x, e)
  | _ -> failwith "test_fun but nothing to do"

(** [make n s1 s2] makes an OUnit test named [n] that expects
    [s2] to evalute to [s1]. *)
let make_fun_c n s1 s2 =
  n >:: (fun _ -> assert_equal (parse s1) (s2 |> interp |> code_part))

(** [make_fun_i n s1 s2] makes an OUnit test named [n] that expects
    [s2] to evalute to [s1]. *)
let make_fun_i n i s =
  n >:: (fun _ -> assert_equal (VInt i) (interp s))

(** [make n s1 s2] makes an OUnit test named [n] that expects
    [s2] to evalute to [s1]. *)
let make_fun_b n b s =
  n >:: (fun _ -> assert_equal (VBool b) (interp s))


(** This test suite is imperfect in that it only checks the code
    part of closures, not the environment part, for correctness. *)
let tests_fun = [
  make_fun_i "fun id" 1 "(fun x -> x) 1";
  make_fun_i "fun add" 22 "(fun x -> x * 2) 11";
  make_fun_i "fun mult" 23 "(fun x -> x + 12) 11";
  make_fun_b "fun leq" true "(fun x -> x <= 3) 3"; 
  make_fun_b "fun leq" false "(fun x -> x <= 3) 5";
  make_fun_i "fun let" 22 "(fun x -> let y = 3 in x + y) 19";
  make_fun_i "fun if" 22 "(fun x -> if x <= 12 then 20 else 22 ) 19";
  make_fun_i "let fun" 3 "let f = fun x -> x + 2 in f 1 ";
  make_fun_i "if fun1"  22 "if 4 <= 3 then (fun y -> y + 1) 9 
                                     else (fun y -> y * 2) 11";
  make_fun_i "if fun2"  10 "if 2 <= 3 then (fun y -> y + 1) 9 
                                     else (fun y -> y * 2) 11";
  make_fun_i  "fun lexical" 3 
                              "let y = 1 in 
                                  let f = fun x -> x + y in 
                                      let y = 2 in
                                      f  2 ";
              
  make_fun_c "reduce correct"
    "fun y -> y"
    "(fun x -> x) (fun y -> y)";
  make_fun_c "scope correct" (* lexical scope *)
    "(fun b -> b)"
    (* this is the example from the notes, but with
        - [fun a -> a] in place of [0]
        - [fun b -> b] in place of [1],
        - [fun c -> c] in place of [2];
        and with the [let] expressions desugared to functions. *)
    "(fun x -> \
        (fun f -> \
          (fun x -> \
              f (fun a -> a)) \
                (fun c -> c)) \
                (fun y -> x)) \
                (fun b -> b)"
]


let _ = run_test_tt_main ("suite" >:::  tests @ tests_fun)