let get_val default = function
  | None -> default
  | Some x -> x

let rec list_max (lst : 'a list) : 'a option =
  match lst with
  | h :: t ->  begin
      match list_max t with
        | None -> Some h
        | Some l -> Some (max h l)
      end
  | [] -> None
  
   