(* -----Array module how to implement make matrix ----- *)

external make: int -> 'a -> 'a array = "caml_make_vect"
external create: int -> 'a -> 'a array = "caml_make_vect"
external unsafe_set: 'a array -> int -> 'a -> unit = "%array_unsafe_set"

let make_matrix sx sy init =
  let res = create sx [||] in
  for x = 0 to pred sx do
    unsafe_set res x (create sy init)
  done;
  res

(* how to implement init_matrix*)

(** [makek_matrix sy sy f] is the matrix 
   with each value aij initialed with f i j
   RI: sx sy > 0 *)
let make_matrix sx sy f =
  let res = create sx [||] in
  for x = 0 to pred sx do
    let rowy = make sy (f x 0) in
    for y = 1 to pred sy do
      unsafe_set rowy y (f x y)
    done;
    unsafe_set res x rowy
  done;
  res

(**  [a] is a float array represent the float vector *)
let square a = a ** 2.
let divby ~divsor dividend = dividend /. divsor

let norm a = 
  a
  |> Array.map square 
  |> Array.fold_left ( +. ) 0.
  |> sqrt

(* res is a new float array
   the input a is unchanged.*)
let normalize a =
  let lena = norm a in
  Array.map (divby ~divsor:lena) a

(** res is unit
   the input a is changed.*)
let normalize' a =
  let lena = norm a in
  for i = 0 to pred (Array.length a) do
      a.(i) <- a.(i) /. lena
  done

let normloop a =
  let acc =  ref 0. in
  for i = 0 to pred (Array.length a) do
      acc := !acc +. a.(i) ** (2.)
  done;
  !acc

let normalizeloop a =
  let acc' = sqrt (normloop a) in
  for i = 0 to pred (Array.length a) do
      a.(i) <- a.(i) /. acc'
  done