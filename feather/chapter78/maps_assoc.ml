module type Map = sig

  (** [('k, 'v ) t]  is the type of maps binding keys and values. *)
  type ('k, 'v ) t

  (** [insert k v m] is the same map [m] 
      but with an additional binding between [k] and [v]]
      if [k] is already bound in [m], that binding is replaced by
       the binding to [v]  *)
  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t

  (** [find k m] is [Some v] if [k] is bound to [v] in [m],
      and [None] if not *)
  val find : 'k -> ('k, 'v) t -> 'v option

  (** [remove k m] is the same map [m] without any binding of [k].
      if [k] is not bound in [m], then the map is unchanged. *)
  val remove: 'k -> ('k, 'v) t -> ('k, 'v) t
  
  (** [empty] is the empty map. *)
  val empty: ('k, 'v) t

  (** [of_list lst] is a map containing the same bindings as 
      the association list [lst] 
      Require: [lst] does not contain any duplicate keys*)
  val of_list: ('k * 'v) list -> ('k, 'v) t

  (**[bindings m] is an association list containing the same 
      bindings as map [m]. There are not duplicates in the list*)
  val bindings: ('k, 'v) t -> ('k * 'v) list
end

(* ('k, 'v) t -> ('k * 'v) list
  the differences ('k, 'v) and ('k * 'v)
  The main reason is the definition of two type constructors
  type constructor t is parameteried on two type variables. 
      type ('k, 'v ) t
  type constructor list is parameterized on only one type variable. 
      'a list = [] | (::) of 'a * 'a list 
  
  To 'a list, it takes one type variable, if we want it takes twe,
    we have to warp twp type variables into a tuple type like ('k * 'v) 
    which is only one type variable.
  To ('k, 'v) t, it takes two type varialbes, 
    OCaml syntax require us to separate two type variables useing comma 
    in a pair of parentheses. ('k, 'v) looks like a tuple value, which has
    two type variables ---- its first and second

  *)


  module ListMap : Map = struct
    (** AF: [[(k1, v1); (k2, v2); ...; (kn, vn)]] is the map {k1 : v1, k2 : v2,
        ..., kn : vn}. If a key appears more than once in the list, then in the
        map it is bound to the left-most occurrence in the list. For example,
        [[(k, v1); (k, v2)]] represents {k : v1}. The empty list represents
        the empty map.
        RI: none. *)
    type ('k, 'v) t = ('k * 'v) list
  
    (** Efficiency: O(1). *)
    let insert k v m = (k, v) :: m
  
    (** Efficiency: O(n). *)
    let find = List.assoc_opt
  
    (** Efficiency: O(n). *)
    let remove k lst = List.filter (fun (k', _) -> k <> k') lst
  
    (** Efficiency: O(1). *)
    let empty = []
  
    (** Efficiency: O(1). *)
    let of_list lst = lst
  
    (** [keys m] is a list of the keys in [m], without
        any duplicates.
        Efficiency: O(n log n). *)
    let keys m = m |> List.map fst |> List.sort_uniq Stdlib.compare
  
    (** [binding m k] is [(k, v)], where [v] is the value that [k]
         binds in [m].
         Requires: [k] is a key in [m].
         Efficiency: O(n). *)
    let binding m k = (k, List.assoc k m)
  
    (** Efficiency: O(n log n) + O(n) * O(n), which is O(n^2). *)
    let bindings m = List.map (binding m) (keys m)
  end


let pp_string s = "\"" ^ s ^ "\""

let pp_list pp_elt lst =
  let pp_elts lst = 
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ (pp_elt h)
      | h1 :: (_h2 :: _t as t') ->
          if n = 100 then acc ^ "..." (*stop printing long list*)
          else loop (n + 1) (acc ^ (pp_elt h1) ^ ";") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

let lst1 = [1; 2; 3; 4]
let lst1_str = pp_string (pp_list string_of_int lst1)

let () = print_endline lst1_str

(* Ounit2 assert_equal has ?cmp 
   using assert_equal with ~cmp:cmp_lists_like_sets*)
let cmp_lists_like_sets lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2




(* In Ounit2,  *)