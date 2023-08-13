type student = {
  name : string ;
  year : int ;
};;

let wyx = {
  name = "wang yx";
  year = 2023 ;
}

let print_studnet_info s = 
     print_endline (s.name ^ " " ^ string_of_int s.year) ;;

let name_of_year s = 
  match s with 
  | {name ; year} -> name ^ " '" ^ string_of_int (year mod 100);