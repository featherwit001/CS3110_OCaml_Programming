let even (x : int ) : bool = 
        x mod 2 = 0 
;;

let sum_if_true (test: int -> bool) first second = 
      ( if test first then first else 0  )
    + ( if test second then second else 0 ) 
;;


let x = sum_if_true even 3 4;;
print_int (sum_if_true even 5 6 );;

print_string "\nhelloword\n";;

let sum_if_true  x y = 
  if (fun x y-> x > y ) x y then x + y else 0;;

type student = {
  name : string ;
  year : int ;
};;

let wyx = {
  name = "wang yx";
  year = 2023 ;
}

let print_studnet_info s = 
     print_endline (s.name ^ " " ^ string_of_int s.year) ;;


type a = { x : int  ;  y : int };;  
type b = {x : int;  y : int };;   

let result = { x = 3;  y = 4};; 