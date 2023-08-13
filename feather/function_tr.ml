let square x = x * x ;;
let result = succ @@ square @@ succ 5;;


(* 
   tail recursive 
   rewrite factorial into tail rescursive function
*)

let rec fact n = 
  if n = 0 then 1 else n * fact(n - 1);;


(* step 1 redefine a auxiliary function with acc*)
let rec fact_aux n acc = ();;

(* step 2 define tail recursive function using auxiliary function *)
let rec fact_tr n = fact_aux n 1;;

(* step 3 change helper functino to return the accumulator in the base case *)
let rec fact_aux n acc = 
 if n = 0 then acc else ()
;;

(* step 4 change the recursive case *)
let rec fact_aux n acc = 
  if n = 0 then acc else fact_aux (n - 1) (n * acc)
;;

(* 
Important: move the remaining calculation to 
           the argument of recursive function like acc.   
*)

let rec fact_tr n = fact_aux n 1;;

(* the conditions for tail recursive *)
(* Recursive call is the last operation of the function, 
   and 
   the return value of the recursive call is directly returned by the function 
   without any further computations. 
*)