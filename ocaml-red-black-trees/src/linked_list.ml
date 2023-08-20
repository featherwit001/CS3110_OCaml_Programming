(* Types *)
type 'a linked_list = Empty | Node of 'a * 'a linked_list


(* Membership *)
let rec member (l: 'a linked_list) (x: 'a) : bool = 
    match l with
        | Empty        -> false
        | Node (x', l) -> x' = x
                       || member l x 


(* Insert *)
let rec insert (l: 'a linked_list) (x: 'a) : 'a linked_list =
    match l with
        | Empty         -> Node (x, Empty)
        | Node (x', l') -> if      x' < x 
                           then    Node (x', insert l' x)
                           else if x' > x 
                           then    Node (x,  Node (x', l'))
                           else    Node (x', l')


(* Delete *)
let rec delete (l: 'a linked_list) (x: 'a) : 'a linked_list = 
    match l with
        | Empty         -> Empty
        | Node (x', l') -> if   x' = x 
                           then l'
                           else Node (x', delete l' x)


(* Tests *)
let l = insert Empty 5;;
let l = insert l 10;;

assert ( member l 5  = true );;
assert ( member l 10 = true );;
assert ( member l 15 = false );;

let l = delete l 10;;
assert ( member l 5  = true );;
assert ( member l 10 = false );;
assert ( member l 15 = false );;
