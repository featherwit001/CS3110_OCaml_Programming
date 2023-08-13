type ptype = TFire | TWater | TNormal

type peff = ENormal | ENotvery | ESuper

(* let _x  = Normal;; *)


let mut_of_eff = function
    | ENormal -> 1.
    | ENotvery -> 0.5
    | ESuper -> 2.

let eff = function 
    | (TFire, TWater) -> ENotvery
    | (TWater, TFire) -> ESuper
    | (p , q) when p = q -> ENotvery
    | _ -> ENormal

type mon = {
    name : string;
    hp : int;
    ptype : ptype;
}

let charmander = {
  name = "Charmander";
  hp = 39;
  ptype = TFire;
}
