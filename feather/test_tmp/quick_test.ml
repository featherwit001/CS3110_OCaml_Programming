open QCheck

let random_string_gen (st : Random.State.t) : string =
  let length = Random.State.int st 10 in 
  let chars = "abcdefghijklmnopqrstuvwxyz" in 
  let rec generate_string acc remaining_length =
    if remaining_length = 0 then acc
    else
      let random_char = String.get chars (Random.State.int st (String.length chars)) in
      generate_string (acc ^ String.make 1 random_char) (remaining_length - 1)
  in
  generate_string "" length 

let custom_string_gen = make random_string_gen 

let prop_length_twice s = String.length s * 2 = String.length (s ^ s) 

let _ =
  let test = Test.make ~count:1000 custom_string_gen prop_length_twice in
  QCheck_runner.run_tests [test]
