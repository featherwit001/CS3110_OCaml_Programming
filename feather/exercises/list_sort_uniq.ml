let interior string_of_elt h t =
  t
  |> List.map string_of_elt 
  |> List.fold_left (fun acc elt -> acc ^ "; " ^ elt) (string_of_elt h)

let string_of_lst string_of_elt = function
  | [] -> "[]"
  | h :: t -> "[" ^ interior string_of_elt h t ^ "]"

let print_list string_of_elt lst = 
  Printf.printf "%s\n" (string_of_lst string_of_elt lst)

let print_int_list = print_list string_of_int


(* to do but it turns out that sort_uniq is mostly for the efficency rather than corecct.contents
   the simple incr or decr merge sort could achieve sort and unique
   when do merge, the first elt of result list must be compared with the new elt to be added*)
let rec rev_append l1 l2 =
  match l1 with
    [] -> l2
  | a :: l -> rev_append l (a :: l2)

(* l1 l2 is decr,out put is incr *)
let rec rev_merge_rev cmp l1 l2 accu =
  match l1, l2 with
  | [], l2 -> rev_append l2 accu
  | l1, [] -> rev_append l1 accu
  | h1::t1, h2::t2 ->
      let c = cmp h1 h2 in
      if c = 0 then rev_merge_rev cmp t1 t2 (if List.hd accu = h1 then accu else h1 :: accu)
      else if c > 0
      then rev_merge_rev cmp t1 l2 (if List.hd accu = h1 then accu else h1 :: accu)
      else rev_merge_rev cmp l1 t2 (if List.hd accu = h2 then accu else h2 :: accu)


(* Sorry. I'm wrong!
   The main reason is that list is a stack. 
   each merge will reverse the order 
   and merge could not compare List.hd accu with new value waiting to add,
   The same key will only exist twice, and they will appear in two separate lists.
   the comparison between 'h1' and 'h2' can eliminate this possibility.
*)