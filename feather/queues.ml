
module ListQueueImpl  = struct
  type 'a queue = 'a list

  let empty = []

  let enqueue x q= 
    q @ [x] (* linear time :( *)
    
  let peek = function
    | [] -> None
    | x :: _ -> Some x

  let dequeue = function
    | [] -> None
    | _ :: q -> Some q

end


(* module without explicit type it will be infered a specific type.like: 
  
sig
  type 'a queue = 'a list
  val empty : 'a list
  val enqueue : 'a -> 'a list -> 'a list
  val peek : 'a list -> 'a option
  val dequeue : 'a list -> 'a list option
end   

this is module ListQueue's type

*)

module TwoListQueueImpl = struct
  (* if front =[a; b; c] and back = [f; e; d] 
    represent the queue is a b c d e f  
    if front is [], back must also be [] 
  *)
  
  type 'a queue = {
    front : 'a list;
    back : 'a list;
  }

  let empty = {
    front = [];
    back = [];
  }

  let peek = function
    | {front = []; _} -> None
    | {front = x :: _; _} -> Some x

  let enqueue x = function
    | {front = []; _} ->  {front = [x]; back = []}
    | q ->  {q with back = x :: q.back}  (* constant time !!!*)

  let dequeue = function
    | {front = []; _} -> None
    | {front = _x :: []; back } -> 
          Some {front = List.rev back ; back = []}
    | {front = _:: q; back} -> 
          Some {front = q; back}
end

(* by the way, you have to add documentation comment 
              to describe the behaviors of these values*)
module type QueueSig = sig
  type 'a queue

  val empty: 'a queue

  val peek: 'a queue -> 'a option

  val enqueue: 'a -> 'a queue -> 'a queue
  
  val dequeue: 'a queue -> 'a queue option 

end

module ListQueue: QueueSig =  ListQueueImpl

module TwoListQueue: QueueSig = TwoListQueueImpl
