module ListStack  = struct
  type 'a liststack = 'a list

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

module ListQueue = struct
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

let x = ListQueue.(empty |> enqueue 42 |> dequeue);;

(* let x' = ListQueue.(empty |> enqueue 42 |> dequeue |> enqueue 43);; *)

let ( |> ) x f =
    f x

    (* option.map *)
let ( >>| ) opt f =
  match opt with
  | None -> None
  | Some x -> Some (f x)

let x'' : int  list option = 
  ListQueue.(empty |> enqueue 42 |> dequeue >>| enqueue 43)

  (* option.bind *)
let ( >>= ) opt f =
  match opt with
  | None -> None
  | Some x -> f x

(* if there is not the annotation, ">>l" could be useful,
   but the result will be int list option option.
   it is imposible to use >>| to do anything on an object with type x x option option
   we have to avoid it. 
   
   So ">>=" is created, 
   it will take opt(Some x or None) and f and return None or f x.
   the result is strapped. 
   
   if the function return an option, like dequeue, 
   the result we want will only be wrapped with one option.
 *)
let x3  = 
  ListQueue.(empty |> enqueue 42 |> dequeue >>| enqueue 43 >>| dequeue )

let x4  = 
  ListQueue.(empty |> enqueue 42 |> dequeue >>| enqueue 43 >>= dequeue )


  (* pipeline can be arranged to separated lines *)
let q : int list option = 
  let open ListQueue in 
  empty 
  |> enqueue 42 
  |> dequeue 
  >>| enqueue 43 
  >>= dequeue 


  (* Summary use 
      >>| is used to unwrap option to get the result to 
        a function taking x without option and 
            returning result WITHOUT option 
        

      >>=  is used to unwrap option to get the result to 
        a function taking x without option and 
            returning result WITH option   
        to aviod nested option 

      both of them are designed to maintain only one level option
          to unwrap an option for funcitons which takes arguments without option
      >>| is for functions whose result is NOT wrapped in a option. 
      >>= is for functions whose result is wrapped in a option.     
      
      mnemonic:
      >>|: | looks like a wrap,
              which means its output wrapped additionally in a option.
              suitbale for those funciotns whose results come without option.
      >>=: = looks like two detached parentheses from an input argument, 
              which means its output is naked---without a option wrapped
              suitable for some functions whose reuslts come with one layer of  option.
  *)

      