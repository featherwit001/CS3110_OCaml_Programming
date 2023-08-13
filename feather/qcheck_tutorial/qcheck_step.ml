(* int QCheck.Gen.t *)
QCheck.Gen.int ;;
(* ?gen:char QCheck.Gen.t -> string QCheck.Gen.t *)
QCheck.Gen.string;;
(* int -> int -> int QCheck.Gen.t *)
QCheck.Gen.int_range;;
(*int QCheck.Gen.t -> 'a QCheck.Gen.t -> 'a list QCheck.Gen.t  *)
QCheck.Gen.list_size;;
(* 'a QCheck.Gen.t -> 'a list QCheck.Gen.t *)
QCheck.Gen.list;;


(* ?rand:Random.State.t -> n:int -> 'a QCheck.Gen.t -> 'a list *)
QCheck.Gen.generate;;
(* ?rand:Random.State.t -> 'a QCheck.Gen.t -> 'a *)
QCheck.Gen.generate1;;


(* 'a QCheck.Gen.t -> 'a QCheck.arbitrary *)
QCheck.make;;
(* int QCheck.arbitrary *)
QCheck.int;;
(* int -> int -> int QCheck.arbitrary *)
QCheck.int_range;;
(* 'a QCheck.arbitrary -> 'a list QCheck.arbitrary *)
QCheck.list;;
(* int QCheck.Gen.t -> 'a QCheck.arbitrary -> 'a list QCheck.arbitrary *)
QCheck.list_of_size;;
(* string QCheck.arbitrary *)
QCheck.string;;
(* string QCheck.arbitrary *)
QCheck.small_string;;
(* 'a QCheck.arbitrary -> ('a -> bool) -> QCheck2.Test.t *)
QCheck.Test.make;;



(* QCheck2.Test.t list -> int *)
QCheck_runner.run_tests;;
