let rec add1 = function
  | [] -> []
  | h :: t -> h + 1 :: add1 t

let rec concat3110 = function
  | [] -> []
  | h :: t -> ( h ^ "3110" ) :: concat3110 t
(* operator precedence *)


let rec transform f  = function
  | [] -> []
  | h :: t -> f h :: transform f t;;

let add1' lst = transform (fun x -> x + 1)  lst;;
let concat3110 lst = transform (fun x -> x ^ "3110") lst;;


(* now we can transform "transform" int "map" *)
 
let rec map f  = function
  | [] -> []
  | h :: t -> f h :: map f t;;

let add1' lst = map (fun x -> x + 1)  lst;;
let concat3110' lst = map (fun x -> x ^ "3110") lst;;

(* and we can have a more decent way to use f *)
(* use the function name directly! *)

let string_of_intlist lst = map (fun x -> string_of_int x) lst;;
let string_of_intlist' lst = map string_of_int lst;;

(* let's review the implement of map *)

let rec map' f = function
  | [] -> []
  | h :: t -> f h :: map' f t

(* the implement using the abstract of high_order_function *)


(* the conditions for tail recursive *)
(* Recursive call is the last operation of the function, 
   and 
   the return value of the recursive call is directly returned by the function 
   without any further computations. 
*)
(* tail_recursive  *)
let rec map'_aux acc f = function
  | [] -> List.rev acc
  | h :: t ->  map'_aux (f h :: acc) f t

let map' f lst = map'_aux [] f lst


