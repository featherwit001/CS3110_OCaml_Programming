(* Types *)
type color   = R | B | BB
type 'a tree = Empty of color | Node of color * 'a tree * 'a * 'a tree


(* Membership *)
let rec member (t: 'a tree) (x: 'a) : bool =
    match t with
        | Empty _            -> false
        | Node (_, l, x', r) -> x' = x
                             || (x' < x && member r x)
                             || (x' > x && member l x) 


(* Insert *)
(* bal_ins_l and bal_ins_r could be combined to bal_ins *)
let bal_ins_l (t: 'a tree) : 'a tree =
    match t with
        | Node (B, Node (R, Node (R, a, x, b), y, c), z, d)
        | Node (B, Node (R, a, x, Node (R, b, y, c)), z, d)
            -> Node (R, Node (B, a, x, b), y, Node (B, c, z, d))
        | n -> n
        
let bal_ins_r (t: 'a tree) : 'a tree =
    match t with
        | Node (B, a, x, Node (R, Node (R, b, y, c), z, d))
        | Node (B, a, x, Node (R, b, y, Node (R, c, z, d))) 
            -> Node (R, Node (B, a, x, b), y, Node (B, c, z, d))
        | n -> n
   
let ins (t: 'a tree) (x: 'a) : 'a tree =
    let rec ins_int (t: 'a tree) : 'a tree = 
        match t with
            | Empty _            -> Node (R, Empty B, x, Empty B)
            | Node (c, l, x', r) -> if      x' < x 
                                    then    bal_ins_r (Node (c, l, x', ins_int r))
                                    else if x' > x 
                                    then    bal_ins_l (Node (c, ins_int l, x', r))
                                    else    Node (c, l, x', r)
    in
    ins_int t
    
let insert (t: 'a tree) (x: 'a) : 'a tree = 
    match ins t x with
        | Empty _            -> failwith "insert error"
        | Node (_, x', l, r) -> Node (B, x', l, r)


(* Delete *)
let rec min (t: 'a tree) : 'a = 
    match t with
        | Empty _                 -> failwith "min error"
        | Node (_, Empty _, x, _) -> x
        | Node (_, l, _, _)       -> min l
      
let node_val (t: 'a tree) : 'a = 
    match t with
        | Empty _           -> failwith "node_val error"
        | Node (_, _, x, _) -> x
       
let left (t: 'a tree) : 'a tree = 
    match t with
        | Empty _           -> failwith "left error"  
        | Node (_, l, _, _) -> l
        
let right (t: 'a tree) : 'a tree =
    match t with
        | Empty _           -> failwith "right error"
        | Node (_, _, _, r) -> r
        
let add_b (t: 'a tree) : 'a tree = 
    match t with
        | Empty B           -> Empty BB
        | Node (R, l, x, r) -> Node (B,  l, x, r)
        | Node (B, l, x, r) -> Node (BB, l, x, r)
        | _                 -> failwith "add_b error"

let rem_b (t: 'a tree) : 'a tree = 
    match t with
        | Empty BB            -> Empty B
        | Node (BB, l, x, r)  -> Node (B, l, x, r)
        | _                   -> failwith "rem_b error"

let is_b (t: 'a tree) : bool = 
    match t with
        | Empty B           
        | Node (B, _, _, _) -> true
        | _                 -> false

let is_r (t: 'a tree) : bool = 
    match t with
        | Node (R, _, _, _) -> true
        | _                 -> false 

let is_bb (t: 'a tree) : bool = 
    match t with
        | Empty BB          
        | Node (BB, _, _, _) -> true
        | _                  -> false

let rec bal_del_l (t: 'a tree) : 'a tree = 
    match t with
        | Node (B, d, y, Node (R, l, z, r)) -> (*sibling is red*)
            if   is_bb d (* exist doule black *)
                (* turn the case to that sibling is black*)
            then Node (B, bal_del_l (Node (R, d, y, l)), z, r) 
            else t (* do nothing without BB *)
        | Node (c, d, y, Node (B, l, z, r)) -> (*sibling is black*)
            if is_bb d then (* exist doule black *)
                if      is_b l && is_b r
                (* sibling's childre are both black, BB upper *)
                then    add_b (Node (c, rem_b d, y, Node (R, l, z, r)))
                else if is_r l && is_b r (* RL *)
                (* just right rotate sibling and change color, make it RR case*)
                then    bal_del_l (Node (c, d, y, Node (B, left l, node_val l, Node (R, right l, z, r))))
                (* l is red or black and r is red, RR *)
                else    Node (c, Node (B, rem_b d, y, l), z, add_b r)
            else t (* do nothing without BB *)
        | n -> n

let rec bal_del_r (t: 'a tree) : 'a tree = 
    match t with
        | Node (B, Node (R, l, z, r), y, d) -> (*sibling is red*)
            if   is_bb d    (* exist doule black, it needs to be balanced *)
                (* turn the case to that sibling is black*)
            then Node (B, l, z, bal_del_r (Node (R, r, y, d))) 
            else t (* do nothing without BB *)
        | Node (c, Node (B, l, z, r), y, d) ->  (*sibling is black*)
            if is_bb d then (* exist doule black, it needs to be balanced *)
                if      is_b l && is_b r  
                (* sibling's childre are both black, BB upper *)
                then    add_b (Node (c, Node (R, l, z, r), y, rem_b d))
                else if is_b l && is_r r  (* LR case , BB can de resolved *)
                (* just left rotate sibling and change color, make it  LL case*)
                then    bal_del_r (Node (c, Node (B, Node (R, l, z, left r), node_val r, right r), y, d))
                (* l is red && r is red or black. LL case BB can be resolved *)
                else    Node (c, add_b l, z, Node (B, r, y, rem_b d)) 
            else t (* do nothing without BB *)
        | n -> n 

let rec del (t: 'a tree) (x: 'a) : 'a tree = 
    let rec del_int (t: 'a tree) : 'a tree = 
        match t with
            | Empty _ -> t  (* empty do nothing *)
            | Node (R, Empty _, x', Empty _) -> 
                (* one red node, 
                   which is the successor node in the last two level of RB tree.
                   del it if need 
                   just replace with Black Empty (Black Leaf) *)
                if   x' = x 
                then Empty B 
                else t
            | Node (B, Empty _, x', Empty _) -> 
                (* one black node, 
                   which is the successor node in the last two level of RB tree.
                   it may be the root but still int the last two level
                   del it if needed will introduce BB*)
                if   x' = x 
                then Empty BB 
                else t
            | Node (_, Empty _, x', Node (_, l, y', r)) 
            | Node (_, Node (_, l, y', r), x', Empty _) ->
                (* one node with only one child, which wiil be met 
                   when find the successor node in the last two level of RB tree
                   del the father with child but blacked
                   del the child, father will be single*)
                if      x' = x 
                then    Node (B, l, y', r)
                else if y' = x 
                then    Node (B, Empty B, x', Empty B)
                else    t
            | Node (c, l, x', r) ->
                (* normal case recursive del and bal as the BST required*)
                if      x' < x 
                then    bal_del_r (Node (c, l, x', del_int r))
                else if x' > x 
                then    bal_del_l (Node (c, del_int l, x', r))
                (* replace the target with the successor node 
                   if it don't have a successor node,
                   it must be matched by the top case 2 to 5*)
                else    let m = min r 
                        in 
                        bal_del_r (Node (c, l, m, (del r m)))
    in
    del_int t
    
let delete (t: 'a tree) (x: 'a) : 'a tree = 
    match del t x with
        | Empty _            -> Empty B
        (* BB root will be remove one black*)
        | Node (_, l, x', r) -> Node (B, l, x', r) 
      

(* Tests *)
let t = insert (Empty B) 5;;
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
