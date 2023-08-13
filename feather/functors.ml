module type X = sig
  val x : int
end

(* Module A's type could be infered to X *)
module A = struct
  let x = 0 
end

(* functor takes in a Module whose type is X, return a Module*)
(* the input Module's type must be explicitly written *)
module IncX = functor (M : X) -> struct
  let x = M.x + 1
end

(* syntax sugar *)
module IncX (M : X) = struct
  let x = M.x + 1
end

(* A's type is infered to X *)
module B = IncX(A)

(* similar to *)
let inc = fun x -> 
  x + 1
let inc x =
  x + 1

module type Y = sig
  val y : int
end

module B : Y = struct
  let y = 2
end


module AddXY = functor (M : X) -> functor (N : Y) -> struct
  let z = M.x + N.y
end

(* syntax sugar *)
module AddXY (M: X) (N: Y) = struct 
  let z = M.x + N.y
end


module C = AddXY (A) (B)

(* similar to *)
let add x y = x + y

let add = fun x y -> x + y

let add = fun x -> fun y -> x + y

