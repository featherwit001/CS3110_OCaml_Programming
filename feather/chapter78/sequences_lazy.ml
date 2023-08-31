module SequencesFibs = struct 
  type 'a sequence = Cons of 'a * (unit -> 'a sequence)

  let hd (Cons (h , _) )= h

  let tl (Cons (_, t)) = t()

  let rec take_aux acc n seq = 
    if n = 0 then acc
    else take_aux (hd seq :: acc) (n - 1) (tl seq)
  let take n sequence = List.rev (take_aux [] n sequence)

  let rec drop n seq = 
    if n = 0 then seq
    else drop (n - 1) (tl seq)

  let nth : int -> 'a sequence -> 'a =
    fun n seq -> take_aux [] (n + 1) seq |> List.hd
  
  let rec sum : int sequence -> int sequence -> int sequence = 
    fun (Cons (h1, t1)) (Cons (h2, t2)) -> 
      Cons (h1 + h2, fun () -> sum (t1()) (t2())) 

  let rec fibs = 
    Cons (1, fun () -> 
      Cons (1, fun () -> 
        sum fibs (tl fibs)))

  let nth_fib n = 
    nth n fibs    
end


module LazyFibs = struct 

  type 'a lazyseq = Cons of 'a * 'a lazyseq Lazy.t

  let hd : 'a lazyseq -> 'a = 
    fun  (Cons (h, _)) -> h

  let tl : 'a lazyseq -> 'a lazyseq = 
    fun (Cons (_, t)) -> Lazy.force t
  
  let rec take_aux acc n seq = 
    if n = 0 then acc
    else take_aux ((hd seq) :: acc) (n - 1) (tl seq)

  (* let rec take_aux acc n (Cons (h, t)) = 
    if n = 0 then acc
      (* attention for the Lazy.force t *)
    else take_aux (h :: acc) (n - 1) (Lazy.force t) *)

  let take : int -> 'a lazyseq -> 'a list =
    fun n seq -> take_aux [] n seq |> List.rev
  let nth : int -> 'a lazyseq -> 'a  = 
    fun n seq ->  take_aux [] (n + 1) seq |> List.hd

  let rec sum (Cons (h1, t1)) (Cons (h2, t2)) =
    Cons (h1 + h2, lazy (sum (Lazy.force t1) (Lazy.force t2)))
  
  let rec fibs = 
    Cons (1 , lazy(
      Cons (1, lazy(
        sum fibs (tl fibs)))))

  let nth_fib n = 
    nth n fibs

end