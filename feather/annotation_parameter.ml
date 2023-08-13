let f ~x ~y = x - y

let res1 = 
  let x = 3 and y = 2 in 
  f ~x ~y


let f2 ~x:x1 ~y:y1 = x1 - y1

let res2 = f2 ~y:2 ~x:3

let bump ?(step = 1) x = x + step;;


let annotation_example ?a ?b ?(c="c") ~d ~e f h =
  let exist =  c ^ d ^ e ^ f ^ h in
  match a, b with
  | Some a, Some b -> a ^ b ^ exist
  | _ -> exist

let res3 = annotation_example ~b:"b" ~d:"d" ~e:"e" "f" "h" 

let bump ?(step = 1) x = x + step;;

let b2 = bump 1
let b4 = bump ~step:3 1

let bump ?step x =
  match step with
  | None -> x * 3
  | Some y -> x + y

let b3 = bump 1
let b5 = bump ~step:3 2

(* bad bump_it 
   bump was infered to step:int -> 'a -> 'b
   it's incompatible with bump defined above 
      whose type is ?step:int -> int -> int*)
let bump_it bump x =
  bump ~step:2 x;;

(* let res4 = bump_it bump 1  (*error *) *)

let bump_it (bump: ?step:int -> int -> int) x = bump ~step:2 x

let res4 = bump_it bump 2


