module type Fraction = sig
  (* A fraction is a rational number p/q, where q != 0.*)
  type t

  (** [make n d] is n/d. Requires d != 0. *)
  val make : int -> int -> t

  val numerator : t -> int
  val denominator : t -> int
  val to_string : t -> string
  val to_float : t -> float

  val add : t -> t -> t
  val mul : t -> t -> t
  val reduce : t -> t

  val make_directly : int-> int -> t

end
let rec gcd a b =
  if b = 0 then a
  else gcd b (a mod b)

module TupleFraction: Fraction = struct
  
  type t = int * int 

  let reduce = function
  | (_p, q) when q = 0 -> failwith "div zero"
  | (p, q)  -> begin 
     let g = gcd (p) (q) in
     let r = (p / g, q / g) in
      match r with
      | (p, q) when q < 0 -> (-p, -q)
      | r -> r
  end
  let make n d = reduce (n, d)

  let numerator = function
    | (p, _q) -> p
  
  let denominator = function
    | (_p, q) -> q

  let to_string = function
    | (p, q) -> "<" ^ (string_of_int p) ^ " / " ^ (string_of_int q) ^ ">"

  let to_float = function
    | (p, q) -> float_of_int p /. float_of_int q
  
  let add r1 r2 = 
    match r1, r2 with
    | (p1, q1) , (p2, q2) -> reduce (p1 * q2 + p2 * q1 , q1 * q2) 
  
  let mul r1 r2 = 
    match r1, r2 with
    |(p1, q1) , (p2, q2) -> reduce (p1 * p2 , q1 * q2)

    let make_directly p q = (p, q)
end

open OUnit
open TupleFraction

(* primitive type is different from module type t
   so assert_equal need some method to compare expect and output *)
let make_tests_frac_make name expect input_arg1 input_arg2 =
  let (p, q) = expect in (name >:: (fun _ -> 
            assert_equal
            (make_directly p q) 
            (make input_arg1 input_arg2) 
            ~printer:to_string))
let tests_frac_make = "tests_frac_make" >::: [
  "div 0" >:: (fun _ -> (assert_raises (Failure "div zero") (fun _ -> make 15 0)));
  make_tests_frac_make " 5 /  15" ( 1, 3)   5   15;
  make_tests_frac_make "-5 /  15" (-1, 3) (-5)  15;
  make_tests_frac_make " 5 / -15" (-1, 3)   5 (-15);
  make_tests_frac_make "42 /  -7" (-6, 1)  42 ( -7);
  make_tests_frac_make "100 / 25" ( 4, 1) 100  25;
  make_tests_frac_make "0 / 1"   ( 0, 1)   0   32;
]

let make_tests_frac_add name expect input_arg1 input_arg2 =
  name >:: (fun _ -> 
            assert_equal
            expect
            (add input_arg1 input_arg2) 
            ~printer:to_string)
let frac1 = make 1 3
let frac2 = make 2 3
let frac3 = make 1 (-3)
let frac4 = make 0 1
let frac5 = make (-34) (53)
let frac6 = make (73) (93)
let frac7 = make 707 4929

let tests_frac_add = "tests_frac_add" >::: [
  make_tests_frac_add "1/3 + 1/3" frac2 frac1 frac1;
  make_tests_frac_add "1/3 - 1/3" frac4 frac1 frac3;
  make_tests_frac_add "-34/53 - 73/93" frac7 frac5 frac6;
]

let make_tests_frac_mul name expect input_arg1 input_arg2 =
  name >:: (fun _ -> 
            assert_equal
            expect
            (mul input_arg1 input_arg2) 
            ~printer:to_string)
let frac_mul1 = make 1 1
let frac_mul2 = make 0 32
let frac_mul3 = make 0 1
let frac_mul4 = make 4 5
let frac_mul5 = make 25 24
let frac_mul6 = make 5 6
let tests_frac_mul = "tests_frac_add" >::: [
  make_tests_frac_mul "1 * 1" frac_mul1 frac_mul1 frac_mul1;
  make_tests_frac_mul "1 * 0" frac_mul3 frac_mul2 frac_mul3;
  make_tests_frac_mul "4/5 * 25/24" frac_mul6 frac_mul4 frac_mul5;
]
let all_tests = "all_tests" >::: [tests_frac_make; 
                                  tests_frac_add;
                                  tests_frac_mul]

let _ = run_test_tt_main all_tests