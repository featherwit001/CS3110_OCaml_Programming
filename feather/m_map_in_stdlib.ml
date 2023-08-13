(* Map Module *)
type day = Mon | Tue | Wed | Thu | Fri | Sat | Sun

let int_of_day = function
  | Mon -> 1
  | Tue -> 2
  | Wed -> 3
  | Thu -> 4
  | Fri -> 5
  | Sat -> 6
  | Sun -> 7

module OrderDay = struct
  type t = day
  let compare day1 day2 =
  int_of_day day1 - int_of_day day2
end

module DayMap = Map.Make(OrderDay)

let m = 
  let open DayMap in
  empty
  |> add Mon "Monday"
  |> add Tue "Tuesday"


open DayMap
let m = add Fri "Friday" m

let e1 = mem Mon m
let e2 = mem Tue m
let e3 = mem Wed m

let v1 = find Mon m
let v2 = find Wed m (* exception *)

let lst = bindings m


