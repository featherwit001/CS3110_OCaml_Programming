type point = float * float ;;

type shape =
    | Circle of {center: point; radius: float}
    | Rectange of {lower_left: point; upper_right: point}
;;

let _c1 = Circle {center = (0., 0.); radius = 1.}
let rect1 = Rectange {lower_left   = (-1. , -1.); 
                      upper_right = (1. , 1.)}
;;

let avg a b =
    (a +. b ) /. 2.
;;
let center s =
    match s with
    | Circle {center; _} -> center
    | Rectange{lower_left; upper_right} -> (* failwith "TODO" *)
        let (x_ll, y_ll) = lower_left in (
        let (x_ur, y_ur) = upper_right in
            (avg x_ll x_ur, avg y_ll y_ur)
        ) 
;;

print_endline (let (x, y)  = (center rect1) in string_of_float x ^ " " ^ string_of_float y);;