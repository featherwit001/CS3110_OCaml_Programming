(* Random.init;;
Random.self_init;;
Random.State.make_self_init;;
Random.State.make;;
Random.set_state;; *)

(* how to use  Random.init  
   input is int that is provided by us
   output is Unit because we have known its seed*)
Random.init 100
let a_int_between_0_and_1000 = [Random.int 1000]


(* how to use  Random.State.make  
   input is int array that is provided by us
   output is Random.State.t is a state from the array provided by us*)
let custom_seed = [| 123; 456; 789 |]
let custom_random_state = Random.State.make custom_seed
let random_number = Random.State.int custom_random_state 100

(* Random.self_init input is () .it gets a seed from system
  input is ()
  output is (). we don't know what is the seed *)
let get_no_seed = Random.self_init ()

(* Random.State.make_self_init input is ()
   output the state used to init Random *)
let get_seed = Random.State.make_self_init () 

(* Random.get_state ()  and Random.State.copy xxstate is used to 
   produce the same pseudorandom  *)
let seed1 = Random.get_state()
let seed2 = Random.( State.copy (get_state ()))

