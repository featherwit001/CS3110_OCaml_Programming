(* Order type *)
type order = Lt | Gt | Eq


(* Module signatures *)
module type Cmp = sig
    type t
    val  cmp : t -> t -> order   
end

module type Tree = sig
    type elt 
    type tree

    val  empty  : tree
    val  member : tree -> elt -> bool
    val  insert : tree -> elt -> tree
    val  delete : tree -> elt -> tree
    val of_list : elt list -> tree
    val bindings : tree -> elt list
    val is_RBTree : tree -> bool
end


(* Integer comparison module *)
module IntCmp = struct
    type t = int

    let cmp (x: t) (x': t) : order =
        if      x < x' 
        then    Lt
        else if x > x'
        then    Gt
        else    Eq
end


(* Functor mapping comparison module to Red Black tree module *)
module MakeRBTree (C: Cmp) : (Tree with type elt := C.t) = struct

    (* Types *)
    type elt   = C.t
    type color = R | B | BB
    type tree  = Empty of color | Node of color * tree * elt * tree

    (* Passthrough functions *)
    let empty : tree = Empty B
    let cmp = C.cmp

    (* Membership *)
    let rec member (t: tree) (x: elt) : bool =
        match t with
            | Empty _            -> false
            | Node (_, l, x', r) -> match cmp x' x with
                                        | Lt -> member r x
                                        | Gt -> member l x
                                        | Eq -> true

    (* Insert *)
    let bal_ins_l (t: tree) : tree =
        match t with
            | Node (B, Node (R, Node (R, a, x, b), y, c), z, d)
            | Node (B, Node (R, a, x, Node (R, b, y, c)), z, d)
                -> Node (R, Node (B, a, x, b), y, Node (B, c, z, d))
            | n -> n
        
    let bal_ins_r (t: tree) : tree =
        match t with
            | Node (B, a, x, Node (R, Node (R, b, y, c), z, d))
            | Node (B, a, x, Node (R, b, y, Node (R, c, z, d))) 
                -> Node (R, Node (B, a, x, b), y, Node (B, c, z, d))
            | n -> n
   
    let ins (t: tree) (x: elt) : tree =
        let rec ins_int (t: tree) : tree = 
            match t with
                | Empty _            -> Node (R, Empty B, x, Empty B)
                | Node (c, l, x', r) -> match cmp x' x with
                                            | Lt -> bal_ins_r (Node (c, l, x', ins_int r))
                                            | Gt -> bal_ins_l (Node (c, ins_int l, x', r))
                                            | Eq -> Node (c, l, x', r)
        in
        ins_int t
    
    let insert (t: tree) (x: elt) : tree = 
        match ins t x with
            | Empty _            -> failwith "insert error"
            | Node (_, x', l, r) -> Node (B, x', l, r)

    (* Delete *)
    let rec min (t: tree) : elt = 
        match t with
            | Empty _                 -> failwith "min error"
            | Node (_, Empty _, x, _) -> x
            | Node (_, l, _, _)       -> min l
      
    let node_val (t: tree) : elt = 
        match t with
            | Empty _           -> failwith "node_val error"
            | Node (_, _, x, _) -> x
       
    let left (t: tree) : tree = 
        match t with
            | Empty _           -> failwith "left error"  
            | Node (_, l, _, _) -> l
        
    let right (t: tree) : tree =
        match t with
            | Empty _           -> failwith "right error"
            | Node (_, _, _, r) -> r
        
    let add_b (t: tree) : tree = 
        match t with
            | Empty B           -> Empty BB
            | Node (R, l, x, r) -> Node (B,  l, x, r)
            | Node (B, l, x, r) -> Node (BB, l, x, r)
            | _                 -> failwith "add_b error"

    let rem_b (t: tree) : tree = 
        match t with
            | Empty BB            -> Empty B
            | Node (BB, l, x, r)  -> Node (B, l, x, r)
            | _                   -> failwith "rem_b error"

    let is_b (t: tree) : bool = 
        match t with
            | Empty B           
            | Node (B, _, _, _) -> true
            | _                 -> false

    let is_r (t: tree) : bool = 
        match t with
            | Node (R, _, _, _) -> true
            | _                 -> false 

    let is_bb (t: tree) : bool = 
        match t with
            | Empty BB          
            | Node (BB, _, _, _) -> true
            | _                  -> false

    let rec bal_del_l (t: tree) : tree = 
        match t with
            | Node (B, d, y, Node (R, l, z, r)) -> 
                if   is_bb d 
                then Node (B, bal_del_l (Node (R, d, y, l)), z, r)
                else Node (B, d, y, Node (R, l, z, r))
            | Node (c, d, y, Node (B, l, z, r)) -> 
                if is_bb d then 
                    if      is_b l && is_b r 
                    then    add_b (Node (c, rem_b d, y, Node (R, l, z, r)))
                    else if is_r l && is_b r 
                    then    bal_del_l (Node (c, d, y, Node (B, left l, node_val l, Node (R, right l, z, r))))
                    else    Node (c, Node (B, rem_b d, y, l), z, add_b r)
                else Node (c, d, y, Node (B, l, z, r))
            | n -> n

    let rec bal_del_r (t: tree) : tree = 
        match t with
            | Node (B, Node (R, l, z, r), y, d) -> 
                if   is_bb d 
                then Node (B, l, z, bal_del_r (Node (R, r, y, d)))
                else Node (B, Node (R, l, z, r), y, d)
            | Node (c, Node (B, l, z, r), y, d) -> 
                if is_bb d then
                    if      is_b l && is_b r 
                    then    add_b (Node (c, Node (R, l, z, r), y, rem_b d))
                    else if is_b l && is_r r 
                    then    bal_del_r (Node (c, Node (B, Node (R, l, z, left r), node_val r, right r), y, d))
                    else    Node (c, add_b l, z, Node (B, r, y, rem_b d))
                else Node (c, Node (B, l, z, r), y, d)
            | n -> n

    let rec del (t: tree) (x: elt) : tree = 
        let rec del_int (t: tree) : tree = 
            match t with
                | Empty _ -> t
                | Node (R, Empty _, x', Empty _) ->
                    if   cmp x' x = Eq 
                    then Empty B 
                    else t
                | Node (B, Empty _, x', Empty _) -> 
                    if   cmp x' x = Eq 
                    then Empty BB 
                    else t
                | Node (_, Empty _, x', Node (_, l, y', r)) 
                | Node (_, Node (_, l, y', r), x', Empty _) ->
                    if      cmp x' x = Eq 
                    then    Node (B, l, y', r)
                    else if cmp y' x = Eq 
                    then    Node (B, Empty B, x', Empty B)
                    else    t
                | Node (c, l, x', r) ->
                    match cmp x' x with
                        | Lt -> bal_del_r (Node (c, l, x', del_int r))
                        | Gt -> bal_del_l (Node (c, del_int l, x', r))
                        | Eq -> let m = min r 
                                in
                                bal_del_r (Node (c, l, m, (del r m)))
        in
        del_int t
    
    let delete (t: tree) (x: elt) : tree = 
        match del t x with
            | Empty _            -> Empty B
            | Node (_, l, x', r) -> Node (B, l, x', r)
        
    let rec of_list_aux acc = function
      | [] -> acc
      | h :: t -> 
        let acc' = insert acc h in
        of_list_aux acc' t
    
    let of_list lst = of_list_aux empty lst

    let rec bindings_aux acc = function
       | Empty _ -> acc
       | Node (_c, l, x, r)-> 
         let acc_r = x :: bindings_aux acc r in
         bindings_aux acc_r l

    let bindings t = bindings_aux [] t
    

    let rec is_ordered = function
        | [] | [_] -> true
        | h1 :: (h2 :: _t' as t) ->
           if cmp h1 h2 = Gt then false
           else is_ordered t

    let is_BSTree tree =
        is_ordered (bindings tree)

    let rec black_height_equal = function
       | Empty B -> (true, 1)
       | Node (_c, l, _x, r) as t-> 
        let eq1, lh  = black_height_equal l in 
        let eq2, rh  = black_height_equal r in
        if eq1 && eq2 
        then (true, (if is_b t then 1 else 0 ) + lh + rh)
        else (false, lh - rh)
       | _ -> failwith "BB still exists"

    let is_root_black tree =
        is_b tree
    
    (* local invariant *)
    let rec is_color_correct = function
       | Empty B -> true
       | Node (R, l, _, r) ->
         is_b l && is_b r && is_color_correct l && is_color_correct r
       | Node (B, l, _, r) ->
        is_color_correct l && is_color_correct r
       | _ -> false (* Empty BB or Node(BB, _)*)
    
    let is_RBTree tree = 
        is_root_black tree
        &&
        is_color_correct tree
        &&
        fst (black_height_equal tree)
        &&
        is_BSTree tree

end

module RBT = MakeRBTree(IntCmp)


(* Tests *)
let empty  = RBT.empty
let insert = RBT.insert
let member = RBT.member
let delete = RBT.delete
let bindings = RBT.bindings
let of_list = RBT.of_list
let is_RBTree = RBT.is_RBTree
