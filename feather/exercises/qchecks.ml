let gen_list_length_5_10_from_0_to_100 = 
    QCheck.Gen.(list_size (int_range 5 10) (int_range 0 100))

let seed = Random.State.make_self_init ()
let list1 = QCheck.Gen.(
            generate1 
              ~rand:seed
              gen_list_length_5_10_from_0_to_100
              )

let list2 = QCheck.Gen.(
            generate
            ~rand:seed
            (list_size (int_range 3 3) gen_list_length_5_10_from_0_to_100) 
            )

let list_len_5_10_value_0_100_arb = QCheck.make gen_list_length_5_10_from_0_to_100


let rec is_even = function
  | [] -> false
  | h :: t -> if h mod 2 = 0 then true else is_even t

let qtests = QCheck.Test.make ~name:"list_even" ~count:100 ~max_fail:50 list_len_5_10_value_0_100_arb is_even

let _ = QCheck_runner.run_tests ~verbose:true  [qtests]

(* ---------------- *)
(** [odd_divisor x] is an odd divisor of [x].
    Requires: [x >= 0]. *)
let odd_divisor x =
  if x < 3 then 1 else
    let rec search y =
      if y >= x then y  (* exceeded upper bound *)
      else if x mod y = 0 then y  (* found a divisor! *)
      else search (y + 2) (* skip evens *)
    in search 3