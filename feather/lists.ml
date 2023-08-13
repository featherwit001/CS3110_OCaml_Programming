type intlist =
  | Nil
  | Cons of int * intlist

let rec lenght = function
  | Nil -> 0
  | Cons (_, t) -> 1 + lenght t


type 'a mylist =
  | Nil
  | Cons of 'a * 'a mylist


let rec mylenght = function
  | Nil -> 0
  | Cons (_, t) -> 1 + mylenght t


type 'a lst = 
  | []
  | ( :: ) of 'a * 'a lst


(* type 'a testlst =
  | Ending
  | ( :: ) of { f1 :'a ; f2: 'a testlst } *)

type 'a ss ={
  f1: 'a;
  f2: int;
}

let ( >> ) a b =  {f1 = a; f2 = b};;

