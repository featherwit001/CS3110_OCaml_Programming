let rec evens = function
  | [] -> []
  | h :: t -> begin
      if h mod 2 = 0 
      then h :: evens t
      else evens t
    end

let rec odds = function
  | [] -> []
  | h :: t -> begin
      if h mod 2 = 1
      then h :: odds t
      else odds t 
    end



let even x = 
    x mod 2 = 0
let odd x =
    x mod 2 = 1

let rec filter p = function
  | [] -> []
  | h :: t -> if p h then h :: filter p t else filter p t

let evens' lst = filter even lst
let odds' lst = filter odd lst



(* filter is a recursive function but not tail-recursive, Let's transform it *)

(* step 1 : create function_aux by copying the original funciton
   added acc to the parameter of the copy of original function*)
let rec filter_aux p _acc = function
  | [] -> []
  | h :: t -> if p h then h :: filter p t else filter p t

  (* step 2 : redefine the original funciton using function_aux 
    attention: instantiate the parameter "acc" to its inital state *)
let rec filter p lst = filter_aux p [] lst

(* step 3 : change function_aux return acc where the base case is *)
let rec filter_aux p acc = function
  | [] -> acc
  | h :: t -> if p h then h :: filter p t else filter p t

(* step 4 : the challenge step --- change the recursive case 
    change the original function's name to funciton_aux then
    move the remaining calculation to the argument acc.   *)

let rec filter_aux p acc = function
  | [] -> acc
  | h :: t -> begin
      if p h 
      then h :: filter_aux p acc t 
      else filter_aux p acc t
    end

let rec filter_aux p acc = function
  | [] -> acc
  | h :: t -> filter_aux p (if p h then h :: acc else acc) t
    
(* but there is a little problem *)

(* reverse the acc. 
   it only happens once 
   when the recsive function reach the [] from left to right.
   it don't increase complexity 
   *)
let rec filter_aux p acc = function
  | [] -> List.rev acc
  | h :: t -> filter_aux p (if p h then h :: acc else acc) t
(* refresh filter  *)
let filter p lst = filter_aux p [] lst;; 

(* AFTER you do a computation that you've made a tail-recursive,
   you might reverse the list as a reuslt to make it what you expect.  *)

(* the conditions for tail recursive *)
(* Recursive call is the last operation of the function, 
   and 
   the return value of the recursive call is directly returned by the function 
   without any further computations. 
*)