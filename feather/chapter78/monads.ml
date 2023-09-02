module type Monad = sig
  type 'a t
  val return : 'a -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end



(* the Maybe Module *)
let x = 1 + (4 / 2)



(* bind but binary bind *)
let propagate_none 
    (op: int -> int -> int option)
    (x : int option)
    (y : int option)
  = 
  match x, y with
  | None, _ | _ , None -> None
  | Some a , Some b -> op a b

(* put output into a box *)
let wrap_output 
    (op : int -> int -> int) (x : int) (y : int) 
  = Some (op x y)

let ( + ) = propagate_none (wrap_output Stdlib.( + ))
let ( - ) = propagate_none (wrap_output Stdlib.( - ))
let ( * ) = propagate_none (wrap_output Stdlib.( * ))

let div (x : int) (y: int) =
  if y = 0 then None else wrap_output Stdlib.( * ) x y

let ( / ) = propagate_none div

(* does type check *)
let x = Some 1 + (Some 4 / Some 2)

module Maybe: Monad = struct
  type 'a t = 'a option

  let return x = Some x

  let ( >>= ) m f = 
  match m with 
  | None -> None
  | Some x -> f x

  (* transform intput form value'a into box containing value 'a t  *)
  let upgrade : (int -> int option) -> (int option -> int option) =
    fun (op: int -> int option) (x : int option) -> x >>= op
  
  let upgrade_easy op x = 
      x >>= op
  
  let ( + ) (x: int option) (y: int option) : int option = 
    x >>= fun a -> 
    y >>= fun b -> 
    return Stdlib.(( + ) a b)

  let ( - ) (x: int option) (y: int option) : int option =
  x >>= fun a ->
  y >>= fun b ->
  return Stdlib.(( - ) a b)

  let ( * ) (x : int option) (y : int option) : int option = 
  x >>= fun a ->
  y >>= fun b ->
  return Stdlib.(( * ) a b)

  let ( / ) (x : int option) (y: int option) : int option = 
  x >>= fun a ->
  y >>= fun b -> 
  if b = 0 then None else return Stdlib.(( / ) a b) 


(* abstract *)
  let binary_bind op x y =
    x >>= fun a ->
    y >>= fun b ->
    op a b 

  let binary_return op x y = return (op x y)

  let ( + )  = binary_bind (binary_return Stdlib.( + )) 
  let ( - )  = binary_bind (binary_return Stdlib.( - )) 
  let ( * )  = binary_bind (binary_return Stdlib.( * )) 
  let ( / ) = binary_bind div

end

(*  restore *)
let ( + ) = Stdlib.( + )
let ( - ) = Stdlib.( - )
let ( * ) = Stdlib.( * )
let ( / ) = Stdlib.( / )

(* Writer Monad *)

let inc x = x + 1
let dec x = x - 1

let inc_log x = (x + 1, Printf.sprintf "Called inc on %i; " x)
let dec_log x = (x - 1, Printf.sprintf "Called dec on %i; " x)


let ( >> ) f g x = x |> f |> g

let id = inc >> dec

module Writer: Monad = struct
  type 'a t = 'a * string

  let return x = (x, "")

  let ( >>= ) m f =
  let (x, s1) = m in 
  let (y, s2) = f x in 
  (y, s1 ^ s2)

  let log name f =
    fun x -> (f x, Printf.sprintf "Called %s on %i; " name x)

  let logable name f =
    fun m ->
    m >>= fun x -> log name f x
  
  let inc' = 
      logable "inc" inc
  let dec' =
      logable  "dec" dec

end


module Compose : Monad = struct
  type 'a t = 'a option

  let return x = Some x

  let ( >>= ) m f = 
  match m with 
  | None -> None
  | Some x -> f x

  let compose f g x = 
    f x >>= fun y -> 
      g y
  
  let ( >=> ) f g x = 
    f x >>= fun y ->
      g y
      
end