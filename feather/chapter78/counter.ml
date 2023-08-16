let counter_outside = ref 0
let next1 = fun () ->
  counter_outside := !counter_outside + 1;
  !counter_outside

 (* each time when we call next2_bad, 
    the let sentence was been evaluated and
    counter_insider was initialed to ref 0 
    so the functionality was broken. *)
let next2_bad = 
  fun () ->
    let counter_inside = ref 0 in
    incr counter_inside;
    !counter_inside

(* next2_good was binded to fun () -> .... 
   its type is unit -> int
   the let sentence that bind a loc to counter_inside is evaluated only once 
   when the next2_good was firstly called. 

   After the first call, next2_good has been binded to a function unit -> int, 
   so the let sentence would not been evaluated forever 
   that makes counter is always the same location.
   *)
let next2_good = 
  let counter_inside = ref 0 in
    fun () ->
    incr counter_inside;
    !counter_inside


