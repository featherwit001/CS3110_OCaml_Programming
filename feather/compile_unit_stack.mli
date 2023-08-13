type 'a t

val empty : 'a t

val is_empty : 'a t -> bool

val push : 'a -> 'a t -> 'a t

val peek : 'a t -> 'a

val pop : 'a t -> 'a t

(* compilation unit *)