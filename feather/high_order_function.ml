(* basic implement *)
let double x = 2 * x;;
let square x = x * x;;
(* not use the exist function to reduce the repeition of similar codes *)
let fourth x = x * x * x * x;; 
let quad x = 2 * 2 * x;;


(* take function as argument *)
let fourth' x = square ( square x );; 
let fourth'' x = x |> square |> square;;

let quad' x = double( double x);;
let quad'' x = x |> double |> double;;


(* high order function ---- produce function as a result*)
let twice f x = f (f x);;
(* let twice' f x = x |> f |> f;; *)

(* use hdf -- this new abstract -- to rewrite fourth and function *)
let fourth''' x  = twice square x;;
let quad''' x = twice double x;;

(* the last step  remove the x *)
(* pay attention to their types, no differences*)
let fourth4 = twice square;;
let quad4 = twice double;;


(* If a function receives fewer parameters than required, 
   it will return a new function, 
   waiting for the remaining parameters 
   to complete the functionality of the original function. 
   quad4 and fourth4 were bonded to a new function produced by the mechanism. 
*)

(* out of this series of stances *)
(* if you have a function that use another function to do sth ,Like:*)

let applyf f x = f x;;
let useaf = applyf double ;;

(* useaf require a number x to finish the functionality of "double x" *)