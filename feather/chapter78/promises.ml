(** A signature for Lwt-style promises, with better names *)
module type PROMISE = sig
  type 'a state =
    | Pending
    | Resolved of 'a
    | Rejected of exn

  type 'a promise

  type 'a resolver 

  (** [make ()] is a new promise and resolver. The promise is pending. *)
  val make : unit -> 'a promise * 'a resolver

  (** [return x] is a new promise that is already resolved with value
      [x]. *)
  val return : 'a -> 'a promise

  (** [state p] is the state of the promise *)
  val state : 'a promise -> 'a state

  (** [resolve r x] resolves the promise [p] associated with [r] with
      value [x], meaning that [state p] will become [Resolved x].
      Requires: [p] is pending. *)
  val resolve : 'a resolver -> 'a -> unit

  (** [reject r x] rejects the promise [p] associated with [r] with
      exception [x], meaning that [state p] will become [Rejected x].
      Requires: [p] is pending. *)
  val reject : 'a resolver -> exn -> unit

  (** [p >>= c] registers callback [c] with promise [p].
      When the promise is resolved, the callback will be run
      on the promises's contents.  If the promise is never
      resolved, the callback will never run. *)
  val ( >>= ) : 'a promise -> ('a -> 'b promise) -> 'b promise
end


module Promises : PROMISE = struct 
  type 'a state = 
    | Pending
    | Resolved of 'a
    | Rejected of exn

  (** RI: the input may not be [Pending] *)
  type 'a handler = 'a state -> unit

  (** RI: if [state <> Pending] then [handlers = []]. *)
  type 'a promise = {
    mutable state : 'a state;
    mutable handlers : 'a handler list
  }

  let enquene 
      (handler : 'a state -> unit) 
      (promise : 'a promise) : unit
    =
    promise.handlers <- handler :: promise.handlers

  type 'a resolver = 'a promise

  let make () = 
    let p = {state = Pending; handlers = []} in p, p

  (** [write_once p s] changes the state of [p] to be [s]. If [p] and
    [s] are both pending, that has no effect. Raises: [Invalid_arg] if
    the state of [p] is not pending. *)
  let write_once p s = 
    (* only one resolver (private and writable) associated to one promise
       so that there is not race condition *)
    if p.state = Pending 
    then p.state <- s 
    else invalid_arg "cannot write twice"

  let return x = {state = Resolved x; handlers = []}

  let state p = p.state

  (** requires: [st] may not be [Pending] *)  
  let resolve_or_reject (r : 'a resolver) (st : 'a state) =
    assert (st <> Pending);
    let handlers = r.handlers in 
    r.handlers <- [];
    write_once r st;
    List.iter (fun f -> f st) handlers
  
  let resolve r x = 
    resolve_or_reject r (Resolved x)

  let reject r  e = 
    resolve_or_reject r (Rejected e)
  
  (* there is still a arg  to be filled *)
  let handler (resolver: 'a resolver) : 'a handler 
    = function 
    | Pending -> failwith "handler RI violated"
    | Rejected exc -> reject resolver exc
    | Resolved x -> resolve resolver x

  (* there is still a new arg i.e. promise.state 
     the function type signature warps 'a state -> unit to 'a handler*)
  let handler_of_callback
      (callback : 'a -> 'b promise)
      (resolver : 'b resolver)
      (* ie a state -> unit to 'a handler *)
      : 'a handler 
    = function
     | Pending -> failwith "handler RI violated"
     | Rejected exc -> reject resolver exc
     | Resolved x ->  
      (* in ( >>= )  if the [p] is resolved , apply the [c] to [x] of [p]*)
      let promise = callback x in
      match promise.state with
      | Resolved y -> resolve  resolver y
      | Rejected exc -> reject resolver exc
      | Pending -> enquene (handler resolver) promise  
      (* who can touch p2' = c1 x1 ? none. the resolver must be trigger by bind user*)

  let ( >>= ) 
      (input_promise: 'a promise)
      (callback: 'a -> 'b promise)
      : 'b promise
    =
    match input_promise.state with
    | Resolved x -> callback x
    | Rejected exc -> {state = Rejected exc ; handlers = []}
    | Pending -> 
      let output_promise, output_resolver = make () in
      enquene (handler_of_callback callback output_resolver) input_promise;
      output_promise

  
end


module type Lwt = sig
  (* [Sleep] means pending.  [Return] means resolved.
     [Fail] means rejected. *)
  type 'a state = Sleep | Return of 'a | Fail of exn

  (* a [t] is a promise *)
  type 'a t

  (* a [u] is a resolver *)
  type 'a u

  val state : 'a t -> 'a state

  (* [wakeup] means [resolve] *)
  val wakeup : 'a u -> 'a -> unit

  (* [wakeup_exn] means [reject] *)
  val wakeup_exn : 'a u -> exn -> unit

  (* [wait] means [make] *)
  val wait : unit -> 'a t * 'a u

  val return : 'a -> 'a t
end



(* synchronous *)
let _ =  ignore(input_line stdin); print_endline "done";;

(* asynchronous *)