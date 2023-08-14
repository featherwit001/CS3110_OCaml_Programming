let x = 1000

let y = ref 3110

let res = x + !y

let _ = y := 3210

(* ------------------- *)
let x = ref 42
let y = ref 42
let z = x

let () = x := 43

let w = !y + !z (* 85 *)

let r1 = ref 3110
let r2 = ref 3110

(* physical equality 
   only use it to check left hand is the alias of right hand 
*)
let () = assert (r1 == r1) 
let () = assert (r1 != r2) 

(* structural equality *)
let () = assert (r1 = r1)
let () = assert (r1 = r2)
let () = assert ((ref 3110) = (ref 3110))
let () = assert ((ref 3110) <> (ref 3210))

