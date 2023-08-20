module type Set = sig
  
  type 'a t
  val empty : 'a t
  val insert : 'a -> 'a t -> 'a t
  val mem : 'a -> 'a t -> bool

end

module ListSet: Set = struct 

  type 'a t = 'a list

  let empty = []

  let mem = List.mem

  let insert k s =
    if mem k s then s else k :: s
end

module RbSet = struct
  
  type color = Red | Blk

  type 'a t = Leaf | Node of (color * 'a * 'a t * 'a t)
  let empty = Leaf

  let rec mem x = function
    | Leaf -> false
    | Node (_, v, l, r) ->
      if x < v then mem x l
      else
      if x > v then mem x r
      else
        true
  (* the exterior lever Node is wrapped by insert_aux *)
  let bal = function
  | Blk, z, Node(Red, y, Node(Red, x, a, b), c), d (* LL *)
  | Blk, z, Node(Red, x, a, Node(Red, y, b, c)), d (* LR *)
  | Blk, x, a, Node(Red, y, b, Node(Red, z, c, d)) (* RR *)
  | Blk, x, a, Node(Red, y, Node(Red, z, b, c), d) (* RL *)
        -> Node (Red, y, Node(Blk, x, a, b), Node(Blk, z, c, d))
  | n -> Node n

  let rec insert_aux x = function
    | Leaf -> Node (Blk, x, Leaf, Leaf) (*root node is black*)
    | Node (c, v, l, r) as n -> 
      if x > v then bal (c, v, l, insert_aux x r)
      else 
      if x < v then bal (c, v, insert_aux x l, r)
      else
        n
  
  let insert x s =
    match insert_aux x s with
    | Leaf -> failwith "impossible"
    | Node (_, v, l, r) -> Node(Blk, v, l, r) 
    (*after rotate, the root node may be red, so turn it to black, 
       to increase the black height by one *)
end