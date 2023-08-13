let rec sum = function
  | [] -> 0
  | h :: t -> h + (sum t) 

let rec concat = function
  | [] -> ""
  | h :: t ->  h ^ (concat t)

(* the two function above has similar structure *) 
(* abstract it ,factor out it, don't duplicate time *)

let rec combine init op = function
  | [] -> init (* base case *)
  | h :: t -> op h (combine init op t) (*oprator to operate h and operated t*)


(* rewrite sum and concat *)

let sum' lst = combine 0 (+) lst;;
let concat' lst = combine "" ( ^ ) lst;;


(* fold_right is similar to combind*)
let rec fold_right f lst acc = match lst with
  | [] -> acc
  | h :: t -> f h (fold_right f t acc)


let rec fold_left' f acc = function
  | [] -> acc
  | h :: t -> 
    let acc'  = f acc h in 
    (fold_left' f acc' t)

    (* tial-recursive funciton *)
let rec fold_left f acc lst = match lst with
  | [] -> acc
  | h :: t -> fold_left f (f acc h) t

(* when the operator isn't associative and commutative*)


(* When we want to use the fold function, *)
(* if the elements of the list do not satisfy the left-to-right operation order, *)
(* it is advisable to REVERSE the list and use fold_left whenever possible. *)
