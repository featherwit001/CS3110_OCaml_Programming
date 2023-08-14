let a = [|1|]

let first = a.(0)

 (* out of bounds *)
(* let second_bad = a.(1) *)

type vec = float array

let v = [|1.; 2.|]

let print_vec v =
  for i = 0 to (Array.length v - 1) do
    print_float v.(i); print_newline ()
  done

let print_vec' v =
let print_elt v = 
    print_float v; print_newline () in
  Array.iter print_elt v

let print_vec'' v =
  Array.iter (Printf.printf "%F\n") v

(** [add_vec v1 v2] is a vector which is the sum of [v1] and [v2].
    Example: [vec_add [|1; 2; 3;|] [|4; 5; 6|] ]  is  [|5; 7; 9|] 
    Require v1 v2 has the same length. *)
let add_vec v1 v2 =
  let len1, len2 = Array.length v1, Array.length v2 in
  if len1 <> len2 then invalid_arg "add_vec len inequality" else
    let res = Array.make len1 0. in
    for i = 0 to len1 - 1 do
       res.(i) <- v1.(i) +. v2.(i)
    done;  (*this ; is matter*)
    res

let add_vec' v1 v2 =
  let len1, len2 = Array.length v1, Array.length v2 in
  if len1 <> len2 then invalid_arg "add_vec len inequality" else
    let elt = fun i -> v1.(i) +. v2.(i)in
    Array.init len1 elt

let add_vec'' v1 v2 =
    Array.map2 ( +. ) v1 v2


let v1 = [|1.; 2.; 3.|]
let v2 = [|4.; 5.;6.|]
let v3 = [|5.; 7.; 9.|]