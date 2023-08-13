module type Set = sig
  (** ['a t] is the type of a set whose elements have type ['a] *)
  type 'a t

  (** [empty] is the empty set  *)
  val empty : 'a t

  (** [size s] is the number of elements in set [s]*)
  val size : 'a t -> int

  (* [add x s] is a set containing all the elements of [s] 
     as well as element [x] *)
  val add : 'a -> 'a t -> 'a t

  (* [mem x m ] is [ture] if [x] is an element of [m] *)
  val mem : 'a -> 'a t -> bool
  (* [union s1 s2] is the set containing both the elements
     in [s1] and [s2]*)
  val union : 'a t -> 'a t -> 'a t
  (** [string f s] is a representation of [s] as a string using
      [f] to represent elements as strings *)
  val string : ('a -> string )->'a t -> string
end

(* [dedup lst] is a [lst] but with duplicates removed. it alse
   sort the output list.*)
let dedup lst  =
   lst |> List.sort_uniq Stdlib.compare 



(* why we have to provide string_of_elt 
   because it is the container,the elements is composed of primitive *)
let interior string_of_elt  h t =
  t
  |> List.map string_of_elt
  |> List.fold_left (fun acc ele -> acc ^ "," ^ ele) (string_of_elt h) 

let string_of_lst string_of_elt = function
  | [] -> "{}"
  | h :: t -> "{" ^ interior string_of_elt h t ^"}" 
  (* not a recursive function, just split [h] ant [t] 
     to avaid that , appears at the left of string of [h]
     such as {,42,43} *)


module ListSetNoDups: Set = struct
  (** AF: the list [a1; ...; an] represents the set {a1; ...; an}.
      the empty list {[]} represents the empty set.
      RI: the list must not contain duplicates  *)
  type 'a t = 'a list
  let rep_ok lst = 
      if List.length lst = List.length (dedup lst) 
        then lst
        else failwith "RI"

  let empty = 
    rep_ok []
  let mem x lst = 
    List.mem x (rep_ok lst)
  let size lst = 
    List.length (rep_ok lst)
  let add x s= 
    let s = rep_ok s in 
    if mem x s then s else x :: s
  let union s1 s2 =
    (rep_ok s1) @ (rep_ok s2) |> dedup
  let string f s = 
      string_of_lst f (rep_ok s)
end




module ListSetDups: Set = struct
  (** AF: the list [a1; ...; an] represents the set {b1; ...; bm},
      where the {b1; ...; bm} is the same list as [a1; ...; an]
      but with any duplicates removed.
      the empty list {[]} represents the empty set.
      RI: none. the list may contain duplicates  *)
  type 'a t = 'a list
  let empty = []
  let mem = List.mem
  let size s =
    s |> dedup |> List.length
  let add = List.cons
  let union = List.append
  
  let string f s = 
    s |> dedup |> string_of_lst f

end


(* AF: abstract function 
      ---interpret the represetation type as the dataabstraction
      it's map in mathematical terms.
   RI: representation invariant
      ---determine which values of representation is meaningful
      *) 