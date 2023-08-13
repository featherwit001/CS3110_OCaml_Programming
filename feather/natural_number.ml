type nat =
  | Zero
  | Succ of nat

let zero = Zero
let one = Succ zero
let two = Succ one
let three = Succ two
let four = Succ three

let iszero = function
  | Zero -> true
  | Succ _ -> false

let pred = function
  | Zero -> failwith "pred Zero is undefined"
  | Succ n -> n


let rec nat_of_int = function
  | Zero -> 0
  | Succ n  -> 1 + nat_of_int n

let rec int_of_nat_aux acc = function
  | Zero -> acc
  | Succ n  -> int_of_nat_aux (acc + 1)  n

let int_of_nat n = int_of_nat_aux 0 n


let rec nat_of_int = function
  | 0 -> Zero
  | i when i > 0 -> Succ (nat_of_int (i - 1))
  | _ -> failwith "int_of_nat is undefined on nagetive ints"

let rec nat_of_int_aux acc = function
  | 0 -> acc
  | i when i > 0 -> nat_of_int_aux (Succ acc) (i - 1)
  | _ -> failwith "int_of_nat is undefined on nagetive ints"

let nat_of_int i = nat_of_int_aux Zero i


  (* mutual recsive function *)
let rec even = function
  | Zero -> true
  | Succ n -> odd n
and odd = function
  | Zero -> false
  | Succ n -> even n