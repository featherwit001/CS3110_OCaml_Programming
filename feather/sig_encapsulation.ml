module ListStackImpl  = struct
  type 'a stack = 'a list

  let empty = []

  let push x s =
    x :: s
  
  let peek = function
    | [] -> failwith "Empty"
    | x :: _ -> x

  let pop = function
    | [] -> failwith "Empty"
    | _ :: s -> s

end

module type StackSig = sig
  (** ['a stack ] is the representation type of the stack *)
  (* add the type value 
        which is defined specifically inside of a module structure*)
  type 'a stack
  val empty : 'a stack

  val push : 'a -> 'a stack -> 'a stack

  val peek : 'a stack -> 'a

  val pop : 'a stack -> 'a stack
end

module ListStack : StackSig = ListStackImpl

let x : int ListStack.stack = ListStack.(empty |> push 42)

(* let y : int ListStack.stack = [42] error *)

(* not encapsulated *)
let z : int ListStackImpl.stack = [42]


module MyStack : StackSig = struct
  type 'a stack =
  | Empty
  | Entry of 'a * 'a stack

  (* this is why Constructor is named Constructor. *)
  let empty = Empty

  (* Constructor build a new object. *)
  let push x s =
    Entry (x, s)

  let peek  = function
    | Empty -> failwith "Empty"
    | Entry (x, _) -> x
  let pop  = function
    | Empty -> failwith "Empty"
    | Entry (_ , s) -> s
end