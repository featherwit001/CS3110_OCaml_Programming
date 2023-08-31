let rec ones = 1 :: ones
(* val ones : int list = [1; <cycle>] *)


let rec a = 0 :: b and b = 1 :: a
(* val a : int list = [0; 1; <cycle>]
   val b : int list = [1; 0; <cycle>] *)

(** [from n] is the infinite list [[n; n + 1; n + 2; ...]]. *)
(* error! evaluation immediately *)
(* let rec from n = n :: from (n + 1) *)


(* error! 
   In the definition of a recursive value, 
   we are not permitted to use the value 
   before it is finished being defined *)
(* let rec nats = 0 :: List.map (fun x -> x + 1) nats *)


type 'a mylist = Nil | Cons of 'a * 'a mylist


(*  still can’t define nats *)
type 'a sequence_fail = Cons of 'a * 'a sequence_fail
let rec from n = Cons (n, from (n + 1))


(* What we need is a way to pause evaluation *)
(* using the function is a value, puase the evaluation *)

type 'a sequence = Cons of 'a * (unit -> 'a sequence)
let rec from n = Cons (n, fun () -> from (n + 1))
let nats = from 0


(** [hd s] is the head of [s] *)
let hd (Cons (h, _)) = h

(** [tl s] is the tail of [s] *)
let tl (Cons (_, t)) = t ()

(** [take n s] is the list of the first [n] elements of [s] *)
let rec take n s =
  if n = 0 then [] else hd s :: take (n - 1) (tl s)

(** [drop n s] is all but the first [n] elements of [s] *)
let rec drop n s =
  if n = 0 then s else drop (n - 1) (tl s)

let rec from n = Cons (n, fun () -> from (n + 1))
let nats = from 0

let hd (Cons (h, _)) = h
let tl (Cons (_, t)) = t ()

let rec take_aux acc n s = 
  if n = 0 then acc 
           else take_aux ((hd s) :: acc) (n - 1) (tl s)

let take n s = List.rev (take_aux [] n s )

let rec drop n s =
  if n = 0 then s else drop (n -  1) (tl s)  

let rec square (Cons (h, t)) =
    Cons (h * h , fun () -> square (t()))

let rec sum (Cons (h1, t1)) (Cons (h2, t2)) =
  Cons (h1 + h2, fun () -> sum(t1()) (t2()) )

let rec map f (Cons (h, t)) = 
  Cons ( f h , fun () -> map f (t()))

let rec map2 f (Cons (h1, t1)) (Cons (h2, t2)) =
  Cons ( f h1 h2, fun () -> map2 f (t1()) (t2()))  

(* rec means use itself in the definiftion but not break it up. *)
let rec nats = Cons (0, fun () -> map (fun x -> x + 1) nats)



(* the Fibonacci sequence <1; 1; 2; 3; 5; 8; ...>. *)
(* If we take the tail of it, we get <1; 2; 3; 5; 8; 13; ...>.  *)
(* If we sum those two sequences, we get <2; 3; 5; 8; 13; 21; ...>.  *)

let rec fibs = 
  Cons (1, fun () -> 
    Cons (1, fun () -> 
      sum fibs (tl fibs)))

let fib30long = take 30 fibs |> List.rev |> List.hd

let fib30lazy = lazy (take 30 fibs |> List.rev |> List.hd)

let fib30 = Lazy.force fib30lazy


type 'a lazysequence = Cons of 'a * 'a lazysequence Lazy.t