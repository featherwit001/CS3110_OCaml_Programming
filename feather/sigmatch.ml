module type IntFun = sig
  val f : int -> int
end

module SuccFun : IntFun = struct
  let f x = x + 1
end

module  IdFun: IntFun = struct
  let f x = x 
end

