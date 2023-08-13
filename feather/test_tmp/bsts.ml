(* incredible definition! *)
type ('a, 'b) tree =
  | Leaf
  | Node of ('a * 'b) * ('a , 'b) tree * ('a , 'b) tree (* "," and "*" are different *)


let rec is_bst_helper compare_key min max = function
  | Leaf -> true
  | Node ((key, _value), l, r) -> begin
      let is_min_key_max = compare_key key min > 0 && compare_key key max < 0 in 
      is_min_key_max 
      && (is_bst_helper compare_key min key l) 
      && (is_bst_helper compare_key key max r)
    end
let compare_int a b =
    a - b

(* the keys must be different *)
let is_bst compare_key t = is_bst_helper compare_key (-10000) 10000 t

(* take care of int overflow *)

(* the keys must be different *)
let rec find_bst compare_key key = function
  | Leaf -> None
  | Node ((k, v), l, r) -> begin
    let order = compare_key key k in 
    match order with
    | 0 -> Some v
    | ord when ord < 0 -> find_bst compare_key key l
    | ord when ord > 0 -> find_bst compare_key key r
    | _ -> failwith "order impossible"
  end


module type MyOrderType = sig
  type t
  val compare : t -> t -> int
end


module type BstMap = sig
  type key
  type 'a t
  val empty : 'a t
  val is_empty: 'a t -> bool

  (* the keys added must be different. 
     if add the key existed in the map, 
     the new value will overwirte the old one*)
  val add : key -> 'a -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val find: key -> 'a t -> 'a
  val remove: key -> 'a t -> 'a t
  val binding: 'a t -> (key * 'a) list
  val binding_h: 'a t -> (key * 'a * int) list

end


module Make (Ord: MyOrderType) =  struct
  type key = Ord.t

  type 'a t =  
    |Empty 
    | Node of {l: 'a t; k: key; d: 'a; r: 'a t; h: int}
  
  let empty = Empty
  let is_empty = function
    | Empty -> true
    | _ -> false
  
  let height = function
    | Empty -> 0
    | Node {h; _} -> h

  let create l k d r =
    Node {l; k; d; r; h = (max (height l) (height r)) + 1} 
  
  let _sright_rotate = function
    | Empty -> failwith "right_rotate root_empty"
    | Node {l = Empty; _} -> failwith "right_rotate leftchild_empty"
    | Node {l; k; d; r; _} -> 
      match l with
      | Empty -> failwith "invalid_arg right_rotate"
      | Node {l = ll; k = lk; d = ld; r = lr;_} ->
        create ll lk ld (create lr k d r)
  let _left_rotate = function
    | Empty -> failwith "left_rotation root_empty"
    | Node {r = Empty; _} -> failwith "left_rotate rightchild_empty"
    | Node {l; k; d; r; _} ->
      match r with
      | Empty -> failwith "invalid arg right_rotate"
      | Node {l = rl; k = rk; d = rd; r = rr; _} ->
        create (create l k d rl) rk rd rr

  let bal l k d r = 
    let hl = height l and hr = height r in
    if hl - hr > 1 then begin
      match l with
      | Empty -> failwith "invalid_arg bal l"
      | Node {l = ll; k = lk; d = ld; r = lr; _} ->
        if height ll >= height lr then 
          (* LL right rotate root node*)
          create ll lk ld (create lr k d r)
        else begin
          (* LR left-rotate left child and then right rotate root node*)
          match lr with
          | Empty -> failwith "invalid_arg bal lr"
          | Node {l = lrl; k = lrk; d = lrd; r = lrr; _} ->
            create (create ll lk ld lrl) lrk lrd(create lrr k d r)
          end
      end 
    else if hr - hl > 1 then begin
      match r with
      | Empty -> failwith "invalid_arg bal r"
      | Node {l = rl; k = rk; d = rd; r = rr; _} ->
        if height rr >= height rl then
          (* RR left rotate root node*)
          create (create l k d rl) rk rd rr
        else 
          (* RL right rotate right child and then left rotate  rote node*)
          match rl with
          | Empty -> failwith "invalid_arg bal rl"
          | Node {l = rll; k = rlk; d = rld; r = rlr; _} ->
            create (create l k d rll) rlk rld (create rlr rk rd rr)
      end 
    else 
      Node {l; k; d; r; h = (max hl hr) + 1}
    


  let rec add x data = function
    | Empty -> Node {l = Empty; k = x; d = data; r = Empty; h = 1}
    | Node {l; k; d; r; h} as m ->
      let order = Ord.compare x k in
      if order = 0 then 
        (*Node {...} to construct a new Node*)
        if d = data then m else Node {l; k = x; d = data; r; h} 
      else  
      if order < 0 then 
        let ll = add x data l in 
        (* insert on the left child's left or right subtree*)
        (* the node (k,d) is the lowest unbalanced node or balanced node *)
        if l == ll then m else bal ll k d r 
      else 
        let rr = add x data r in
        (* insert on the right child's left or right subtree*)
        (* the node (k,d) is the lowest unbalanced node or balanced node *)
        if r == rr then m else bal l k d rr

  let rec find x = function
    | Empty -> raise Not_found
    | Node {l; k; d; r; _} -> begin
        let order = Ord.compare x k in
        if order = 0 then  d
        else
          find x (if order > 0 then l else r)
      end
  let rec mem x = function
    | Empty -> false
    | Node {l; k; r; _} -> begin
        let order = Ord.compare x k in
        if order = 0 then true
        else mem x (if order > 0 then r else l)
      end 

  let rec min_binding = function
    | Empty -> failwith "min_binding"
    | Node {l = Empty; k; d; _} ->  (k, d)
    | Node {l; _} -> min_binding l

  let rec min_binding_opt = function
    | Empty -> None
    | Node {l = Empty; k; d; _} -> Some (k, d)
    | Node {l; _} -> min_binding_opt l
  
  let rec max_binding_opt = function
    | Empty -> None
    | Node {r = Empty; k; d; _} -> Some(k, d)
    | Node {r; _} -> max_binding_opt r

  let rec remove_min_binding = function
    | Empty -> failwith "remove_min_binding"
    | Node {l = Empty; r; _} -> r
    | Node {l; _} -> remove_min_binding l
  
  let merge t1 t2 = 
    match t1, t2 with
    | (Empty, t) -> t
    | (t, Empty) -> t
    | (_, _) -> begin
      let (k, d) = min_binding t2 in
        bal t1 k d (remove_min_binding t2) 
      end
  
  let rec remove x = function
    | Empty -> Empty
    | Node {l;k;d;r;_} as m-> begin
      let order = Ord.compare x k in
      if order = 0 then merge l r
      else 
      if order > 0 then 
        let rr = remove x r in if rr == r then m else bal l k d rr
      else
        let ll = remove x l in if ll == l then m else bal ll k d r
    end

  let rec binding_aux acc = function
    | Empty -> acc
    | Node {l; k; d; r; _} ->  (binding_aux ((k, d) :: (binding_aux acc r)) l)

  let binding m = binding_aux [] m

  let rec binding_h_aux acc = function
  | Empty -> acc
  | Node {l; k; d; r; h} ->  (binding_h_aux ((k, d, h) :: (binding_h_aux acc r)) l)

  let binding_h m = binding_h_aux [] m;
end

let string_of_tuple_k_d_h (k, d, h) =
  "(" ^ (string_of_int k) ^"," ^ "\"" ^ d ^ "\"" ^ "," ^ (string_of_int h)  ^")" 

let string_of_tuple_k_d (k, d) =
    "(" ^ (string_of_int k) ^"," ^ "\"" ^ d ^ "\""  ^")" 

let interior string_of_elt h t =
  t
  |> List.map string_of_elt
  |> List.fold_left (fun acc ele -> acc ^ "; " ^ ele ) (string_of_elt h)

let string_of_list string_of_elt = function
  | [] -> "[]"
  | h :: t -> "[" ^ interior string_of_elt h t ^ "]"

let string_of_binding = string_of_list string_of_tuple_k_d

let string_of_binding_h = string_of_list string_of_tuple_k_d_h

