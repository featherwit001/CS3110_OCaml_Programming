(* mutable singly-linked lists *)

(** an ['a node] is a node of a mutable singly-linked list.
    it contains a value of type 'a and optionally has a pointer the next node
    *)
type 'a node = {
  value: 'a;
  (* "mutable x :  int" is equivalent to "x : int ref".
    In Order to simplify the type of x so we prefer to use "mutable".*)
  (* a node need to end at a null like Java. so use option to do that *)
  (* Note that mutable is a property of the field, 
                            rather than the type of the field. 
     In particular, we write mutable field : type, not field : mutable type. *)
  mutable next : 'a node option;
}

(** ['a mlist] is a singly-linked list with elements of type ['a]
      *)
type 'a mlist = {
  mutable first: 'a node option;
}
(** [create v] is a node with value [v] with no link to another node *)
let create v = {
  value = v;
  next = None;
}
(** [singleton v] is a singly-linked list 
    containing only one node wiht value [v] *)
let singleton v =
  {first = Some (create v)}


let insert v lst = 
  match lst.first with
  | None -> 
    lst.first <- Some (create v)
  | was_first ->
    let new_first = create v in
    new_first.next <- was_first;
    lst.first <- Some new_first

let empty_bad = {
  first = None
}

let empty_good () =
  {
    first = None
  }

(* 
  this is a value with value restriction even if it has introduced the _weak subtype
    It means empty_bad is always the same,
    so we need a fun to return a new location wtih content None.
  
  Compare to let empty = Empty in Make module  
  let empty = Empty  that is a type . it just a placeholder 
    and never have a specific data,
    so each time we call empty no matter what it is return, 
    the semantics empty is never broken. (Perhaps it is the same value Empty)
    *)