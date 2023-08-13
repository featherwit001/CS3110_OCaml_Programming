module type Fact = sig
    (** fact [n] is n factorial.  *)
    val fact : int -> int
end

module RecursiveFact: Fact = struct
  let rec fact n = 
    if n = 0
    then 1
    else n * fact (n-1) 
end
  
(* module NotFact: Fact = struct
  let inc x = x + 1
end *)


module TailRecursiveFact: Fact = struct
  let rec fact_aux n acc = 
    if n = 0
    then acc
    else fact_aux (n - 1) (acc * n)

  let fact n = fact_aux n 1
end

(* this struct is infered to this signature:
  "sig val fact_aux : int -> int -> int val fact : int -> int end"
  which contains the signature of the moduel type Fact.

  when we use TailRecursiveFact: Fact, 
      only fact is available. 
          fact_aux is kept hidden inside of the structure, 
                    not exposed to the outer world.
                except that we remove the module type Fact.
*)


let x = TailRecursiveFact.fact 10;

(* let y = TailRecursiveFact.fact_aux 10; *)
