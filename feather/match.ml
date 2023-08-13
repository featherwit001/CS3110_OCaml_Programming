let x = 
  match not true with
  | true -> "nop" 
  | false -> "yap"
;;

let b1 = 
  match ["tyalor";  "swift"] with
  | [] -> "folkore"
  | h :: t -> h
;;

let b2 = 
  match ["tyalor";  "swift"] with
  | [] -> ["folkore"]
  | h :: t -> t
;;


let empty lst = 
  match lst with
  | [] -> true
  | _ :: _ -> false
;;
(*
let emptyf = function
  | [] -> true
  | [h ; ... ; t] -> false;;
*)
let rec sum lst =
  match lst with
  | [] -> 0
  | h :: t -> h + sum t
;;

let rec length lst = 
  match lst with
  | [] -> 0
  | _ :: t -> 1 + length t
;;


(* func append has the same effect as @ *)
let rec append = fun lst1 lst2 -> 
  match lst1 with
  | [] -> lst2
  | h :: t -> h :: append t lst2
;;

let bad_empty lst = 
  match lst with
  | [] -> true
  | _ :: _  -> false
;;