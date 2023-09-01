(* #require "lwt";;  *)
open Lwt
let (p : int Lwt.t), r = Lwt.wait ();;
Lwt.wakeup r 42;;
Lwt.state p;;
(* it will raise a exception*)
(* Lwt.wakeup r 42  *)

let (p : int Lwt.t), r = Lwt.wait ();;
Lwt.wakeup_exn r (Failure "nope");;
Lwt.state p;;



(* #require "lwt.unix";; *)
open Lwt_unix
open Lwt_io;;

ignore(read_line stdin); printl "done";;

(* # <type your own input here> *)

(* When you do type your input, you don’t see it echoed to the screen, 
   because it’s happening in the background. 
   Utop is still executing—it is not blocked—but 
   your input is being sent to that read_line function instead of to utop. 
   When you finally type Enter, the input operation completes, 
   and you are back to interacting with utop. *)

let p = read_line stdin in Lwt.state p;;
p;;
(* #show_val p;; *)

(* # UTop.set_auto_run_lwt false;; *)

(* let rec print_the_string str = Lwt_io.printf "The string is: %S\n" str

Lwt.bind p print_the_string ;; *)


