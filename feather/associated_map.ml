(* associated list *)

let d = [("rectangle", 4); ("nonagon", 9); ("icosagon", 20)]

let insert k v lst = (k, v) :: lst

let rec lookup k = function
  | [] -> None
  | (k', v) :: t-> if k = k' then Some v else lookup k t 

let rec lookup_aux k acc = function
  | _ when acc <> None -> acc
  | [] ->  acc
  | (k', v) :: t-> lookup_aux  k (if k' = k then Some v else acc) t   (*linear time*)

let lookup k lst = lookup_aux k None lst

