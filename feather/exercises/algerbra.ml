(* to do *)

module type Ring = sig
  type t
  val zero : t
  val one : t
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( ~- ) : t -> t
  val string : t -> string
end 

module IntRingRep = struct
  type t = int
  let zero = 0
  let one = 1
  let ( + ) = Stdlib.( + )
  let ( * ) = Stdlib.( * )
  let ( ~- ) = Stdlib.( ~- )
  let string = string_of_int
end

module IntRing : Ring = IntRingRep

module FloatRingRep = struct
  type t = float
  let zero = 0.
  let one = 1.
  let ( + ) = Stdlib.( +. )
  let ( * ) = Stdlib.( *. )
  let ( ~- ) = Stdlib.( ~-. )
  let string = string_of_float
end

module FloatRing: Ring = FloatRingRep


module type Field = sig
  include Ring
  val ( / ) : t -> t -> t
end

module IntFeildRep = struct
  (* watch out include IntRing is error 
    include IntRing, IntFeildRep is
    sig
      type t = IntRing.t
      val zero : t
      val one : t
      val ( + ) : t -> t -> t
      val ( * ) : t -> t -> t
      val ( ~- ) : t -> t
      val string : t -> string
      val ( / ) : int -> int -> int
    end

    include IntRingRep, IntFeildRep is
    sig
      type t = int
      val zero : int
      val one : int
      val ( + ) : int -> int -> int
      val ( * ) : int -> int -> int
      val ( ~- ) : int -> int
      val string : int -> string
      val ( / ) : int -> int -> int
    end
    why type checking encounter an error is clear! the coherence!
    if t is float, IntField will be broken. 

  *)
  include IntRingRep
  
  let ( / ) = Stdlib.( / )
end

module IntField : Field =  IntFeildRep

module FloatFieldRep = struct
  include FloatRingRep 
  let ( / ) = Stdlib.( /. )
end

module FloatField: Field = FloatFieldRep
