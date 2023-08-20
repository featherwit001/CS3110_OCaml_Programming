open Main

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do

      lexbuf
      |> My_parser.prog My_lexer.read
      |> eval
      |> string_of_val
      |> print_endline
      ;
      flush stdout;
    done
  with My_lexer.EOF ->
    exit 0