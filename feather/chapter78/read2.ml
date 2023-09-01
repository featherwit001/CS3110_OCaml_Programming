open Lwt_io

(* let _p =
  Lwt.bind (read_line stdin) (fun s1 ->
    Lwt.bind (read_line stdin) (fun s2 ->
      Lwt_io.printf "%s\n" (s1^s2))) *)

open Lwt.Infix
let p =
  read_line stdin >>= fun s1 ->
  read_line stdin >>= fun s2 ->
  Lwt_io.printf "%s\n" (s1^s2)


let _ = Lwt_main.run p