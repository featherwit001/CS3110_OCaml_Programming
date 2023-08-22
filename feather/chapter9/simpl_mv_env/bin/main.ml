open Interp
open Main
let rec run_once () =
  while true do
  print_string "<calc>:";
  let input = read_line () in
  (* print_string ("\"" ^ input ^ "\""); *)
  try
    let lexbuf = Lexing.from_string input in
      lexbuf
      |> My_parser.prog My_lexer.read
      |> eval
      |> string_of_val
      |> print_endline    
  with My_lexer.Exit_calc ->    exit 0
      | _ -> print_endline "error, try again"; run_once () 

  done

let _  = run_once ()
