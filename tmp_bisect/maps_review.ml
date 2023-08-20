module type Map = sig
  
  type ('k, 'v) t

  val empty : ('k, 'v) t

  (** [insert k v m] is the same map [m] but with a binding of [k] to [v] in [m]
     if [k] exist in [m], then the binding will be replaced by binding to [v]*)
  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  
  (** [find k m] is [Some v] *)
  val find : 'k -> ('k, 'v) t -> 'v option

  val remove : 'k -> ('k, 'v) t -> ('k, 'v) t

  (* [of_list lst] is a map containing the bindings as 
     the associations in the [lst] *)
  val of_list : ('k * 'v) list -> ('k, 'v) t

  (* [bindings m] is a association list containing 
     the bindings in [m]
     no duplicatess*)
  val bindings : ('k, 'v) t -> ('k* 'v) list

end

module ListMap:Map = struct
  
  (* [(k1,v1); (k2,v2); ...]  represent
     {k1:v1; k2:v2; ...}
  *)
  type ('k, 'v) t = ('k * 'v) list

  let empty = []

  (* RI: allow duplicates, 
     but [(k1, v2); (k1,v1); ..]  represent {k1:v2; ...}*)
  let insert k v m = (k, v) :: m

  let find k m = List.assoc_opt k m

  let remove k m = List.filter (fun (k', _) -> k <> k') m
  
  let of_list lst  = lst

  let keys m = 
      m 
      |> List.fold_left (fun acc (k, _v) -> k::acc) []
      |> List.sort_uniq Stdlib.compare
  
  (* in order to use by map, the parameter need to exchange positions *)
  let binding m k = (k, List.assoc k m)

  let bindings m =  
    List.map (binding m) (keys m)
end


module type DirctAddrMap = sig
  
  (** ['v t] is the type of map 
      that binds keys of type int to values of type ['v] *)
  type 'v t
  val create: int -> 'v t
  
  (** [insert i v m] return unit, and change the [m] with adding
     a binding of [i] to [v]
     Requires: [i] is in bounds for [m]. *)
  val insert : int -> 'v -> 'v t -> unit

  (** [find i m] is [Some v], if binding [i] to [v] exist in [m], 
      and [None] if not*)
  val find : int -> 'v t -> 'v option

  (** [remove i m] mutates [m] to remove any binding of [i]. 
      If [i] was not bound in [m], then the map is unchanged. 
      Requires: [i] is in bounds for [m]. *)
  val remove: int -> 'v t -> unit

  (* client need to provide the length of bindings *)
  val of_list: int -> (int * 'v) list -> 'v t

  val bindings: 'v t -> (int * 'v) list
end


module ArrayMap: DirctAddrMap = struct
  type 'v t = 'v option array

  let create len =
    Array.make len None

  let insert i v m =
    m.(i) <- Some v
  
  let find i m =
    m.(i)
  
  let remove i m = m.(i) <- None

  let of_list len lst =
    let m = Array.make len None in 
    List.iter (fun (i, v) -> insert i v m) lst;
    m
  
  let bindings m =
    let lst = ref [] in
    let add_binding i v lst =
      match v with  
      | None -> ()
      | Some x -> lst := (i, x) :: !lst
    in  
    Array.iteri (fun i v -> add_binding i v lst) m;
    !lst
end


(* combine the advantages of ListMap and ListMap with *)
module type TableMap = sig
  type ('k, 'v) t
  val insert : 'k -> 'v -> ('k, 'v) t -> unit
  val find : 'k -> ('k, 'v) t -> 'v option
  val remove : 'k -> ('k, 'v) t -> unit
  val create : ('k -> int) -> int -> ('k, 'v) t
  val of_list : ('k -> int) -> ('k * 'v) list -> ('k, 'v) t
  val bindings : ('k, 'v) t -> ('k * 'v) list 
end

module HashMap: TableMap = struct
  type ('k, 'v) t = {
    hash : 'k -> int;
    mutable size : int;
    mutable buckets : ('k * 'v) list array;
  }

  let capacity tab =
    Array.length tab.buckets

  let load_factor tab =
    float_of_int tab.size /. float_of_int (capacity tab)

  let index k tab =
    (tab.hash k) mod (capacity tab)

  let insert_no_resize k v tab =
    let i = index k tab in
    let old_bucket = tab.buckets.(i) in
    tab.buckets.(i) <- (k, v) :: (List.filter (fun (k', _) -> k' <> k) old_bucket);
    if not (List.mem_assoc k old_bucket) then
      tab.size <- tab.size + 1;
    ()

  let rehash_size tab new_capacity =
    let rehash_binding (k, v) = 
      insert_no_resize k v tab
    in
    let rehash_bucket buc = 
       List.iter rehash_binding buc
    in
    let old_buckets = tab.buckets in
    let new_buckets = Array.make new_capacity [] in
    tab.buckets <- new_buckets;
    tab.size <- 0;
    Array.iter rehash_bucket old_buckets

  let resize_if_need tab =
    let lf = load_factor tab in
    if lf > 2.0 then
      rehash_size tab ((capacity tab) * 2)
    else if lf < 0.5 then 
      rehash_size tab ((capacity tab) / 2)
    else
      ()
  let insert k v tab =
    insert_no_resize k v tab;
    resize_if_need tab

  let find k tab =
    let i = index k tab in
    let target_bucket = tab.buckets.(i) in
    List.assoc_opt k target_bucket

  let remove_no_resize k tab =
    let i = index k tab in
    let old_bucket = tab.buckets.(i) in
    tab.buckets.(i) <- List.remove_assoc k old_bucket;
    if List.mem_assoc k old_bucket then (* if k existed in map*)
      tab.size <- tab.size - 1

  let remove k tab = 
    remove_no_resize k tab;
    resize_if_need tab
  
  let create hash capacity ={
      hash;
      size = 0;
      buckets = Array.make capacity [];
    }
  
  let of_list hash lst =
    let tab = create hash (List.length lst) in 
    List.iter (fun (k, v) -> insert k v tab) lst;
    tab
  
  let bindings tab = 
    let fold_bucket acc bucket =
      List.fold_left 
      (fun acc elt ->  elt :: acc) acc bucket  
    in
    Array.fold_left 
    (fun acc bucket -> fold_bucket acc bucket) 
    [] tab.buckets
    (* []  will be apply into each fold_bucket*)
end