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
 
(* What is map? apply funciton to all the elements of a given structure  *)
let rec map f = function
  | Leaf -> Leaf
  | Node (v, l, r) -> Node(f v, (map f l), (map f r))


let add1 t = map succ t;;


(* What is fold? combine all the elements of a given structure with a given function *)
(* by the way, this fold's order is v, left child, right child, namely preorder traversal*)
let rec fold f acc = function
  | Leaf -> acc
  | Node (v, l, r) -> f v (fold f acc l) (fold f acc r)

let sum t = fold (fun x y z -> x + y + z) 0 t


(* the fold and map could not be rewritten as a tail-recursive function
   becuase they have two recursive branch. 
   It means that out of one recursive call, there must be other computation.   
*)





