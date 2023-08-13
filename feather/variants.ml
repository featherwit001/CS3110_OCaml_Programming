(* type day = Sun | Mon | Tue | Wed | Thu | Fri | Sat
let d = Tue

type primary_color = Red | Green | Blue

let r = Red;; *)

type point = float * float

type shape = 
    | Circle of { center: point; radius: float } 
    | Rectangle of { lower_left: point; upper_right: point };;


let c1 = Circle { center = (0., 0.); radius = 1. }
let rect1 = Rectangle { lower_left = (-1., -1.); upper_right = (1., 1.) }


let avg a b =
    (a +. b ) /. 2.
;;
let center s =
    match s with
    | Circle {center; radius} -> center
    | Rectangle{lower_left; upper_right} -> (* failwith "TODO" *)
        let (x_ll, y_ll) = lower_left in (
        let (x_ur, y_ur) = upper_right in
            (avg x_ll x_ur, avg y_ll y_ur)
        ) 
;;

print_endline (let (x, y)  = (center rect1) in string_of_float x ^ " " ^ string_of_float y);;