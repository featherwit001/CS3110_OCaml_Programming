(* 
  type 'a mylist  =
   | Nil
   | Cons of 'a * 'a mylist
*)


type 'a tree = 
  | Leaf
  | Node of 'a * 'a tree * 'a tree


let t =Node (
          1, 
          Node (2, Leaf, Leaf),
          Node (3, Leaf, Leaf) 
        )

let rec size  = function
  | Leaf -> 0
  | Node ( v, l , r) -> v + size l + size r
 