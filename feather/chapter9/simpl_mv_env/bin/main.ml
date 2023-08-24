open Interp
open Main

let rec read_lines_regex lines =
  let line = read_line () in
  let lines' = line :: " " :: lines in
  let double_semicolon = Str.regexp_string ";;" in 
  try 
    ignore (Str.search_forward double_semicolon line 0);
    List.fold_left ( ^ ) "" (List.rev lines')
  with Not_found -> read_lines_regex lines' 
  
let read_lines () = read_lines_regex []

let string_of_output s =
  "--:: " ^ s

let rec run () =
  let env = empty_env in
  while true do
  try
    print_string "<simPL>:";
    let input = read_lines () in
    print_endline input;
    let lexbuf = Lexing.from_string input in
      lexbuf
      |> My_parser.prog My_lexer.read
      |> eval env
      |> string_of_val
      |> string_of_output 
      |> print_endline    
  with End_of_file  -> print_newline (); exit 0
      (* |My_lexer.Exit_calc ->    exit 0 *)
      | _ -> print_endline "error, try again"; run () 
done


let calc () =
  print_endline "Ctrl + D to exit";
  run ()

let _  = calc ()
