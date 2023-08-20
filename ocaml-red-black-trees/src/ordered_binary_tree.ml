(* Types *)
type 'a tree = Empty | Node of 'a tree * 'a * 'a tree


(* Membership *)
let rec member (t: 'a tree) (x: 'a) : bool = 
    match t with
        | Empty           -> false
        | Node (l, x', r) -> x' = x
                          || member l x 
                          || member r x


(* Insert *)
let rec insert (t: 'a tree) (x: 'a) : 'a tree =
    match t with
        | Empty           -> Node (Empty, x, Empty)
        | Node (l, x', r) -> if      x' < x 
                             then    Node (l, x', insert r x)
                             else if x' > x 
                             then    Node (insert l x, x', r)
                             else    Node (l, x', r)


(* Delete *)
let rec min (t: 'a tree) : 'a =
    match t with
        | Empty              -> failwith "min error"
        | Node (Empty, x, _) -> x
        | Node (l, _, _)     -> min l

let rec delete (t: 'a tree) (x: 'a) : 'a tree =
    match t with
        | Empty  
        | Node (Empty, _, Empty) -> Empty
        | Node (l, x', r)        -> if      x' < x 
                                    then    Node (l, x', delete r x)
                                    else if x' > x
                                    then    Node (delete l x, x', r)
                                    else    let m = min r 
                                            in
                                            Node (l, m, delete r m)


(* Tests *)
let t = insert Empty 5;;
let t = insert t 10;;
let t = insert t 2;;
let t = insert t 12;;
let t = insert t 6;;

assert ( member t 5  = true );;
assert ( member t 10 = true );;
assert ( member t 2  = true );;
assert ( member t 12 = true );;
assert ( member t 6  = true );;
assert ( member t 15 = false );;

let t = delete t 10;;
assert ( member t 5  = true );;
assert ( member t 10 = false );;
assert ( member t 2  = true );;
assert ( member t 12 = true );;
assert ( member t 6  = true );;
assert ( member t 15 = false );;

let t = delete t 6;;
assert ( member t 5  = true );;
assert ( member t 10 = false );;
assert ( member t 2  = true );;
assert ( member t 12 = true );;
assert ( member t 6  = false );;
assert ( member t 15 = false );;
