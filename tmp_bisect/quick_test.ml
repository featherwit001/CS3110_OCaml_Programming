let interior string_of_elt h t =
  t
  |> List.map string_of_elt 
  |> List.fold_left (fun acc elt -> acc ^ "; " ^ elt) (string_of_elt h)

let string_of_lst string_of_elt = function
  | [] -> "[]"
  | h :: t -> "[" ^ interior string_of_elt h t ^ "]"

let print_list string_of_elt lst = 
  Printf.printf "%s\n" (string_of_lst string_of_elt lst)

let print_int_list = print_list string_of_int

let rec length_aux len = function
    [] -> len
  | _::l -> length_aux (len + 1) l

let length l = length_aux 0 l

let rec rev_append l1 l2 =
  match l1 with
    [] -> l2
  | a :: l -> rev_append l (a :: l2)

let rec rev_merge cmp l1 l2 accu =
  match l1, l2 with
  | [], l2 -> rev_append l2 accu
  | l1, [] -> rev_append l1 accu
  | h1::t1, h2::t2 ->
      let c = cmp h1 h2 in
      if c = 0 then rev_merge cmp t1 t2 (h1::accu)
      else if c < 0
      then rev_merge cmp t1 l2 (h1::accu)
      else rev_merge cmp l1 t2 (h2::accu)

let rec rev_merge_rev cmp l1 l2 accu =
  match l1, l2 with
  | [], l2 -> rev_append l2 accu
  | l1, [] -> rev_append l1 accu
  | h1::t1, h2::t2 ->
      let c = cmp h1 h2 in
      if c = 0 then rev_merge_rev cmp t1 t2 (h1::accu)
      else if c > 0
      then rev_merge_rev cmp t1 l2 (h1::accu)
      else rev_merge_rev cmp l1 t2 (h2::accu)
      
let sort_uniq cmp l =
  let rec rev_merge l1 l2 accu =
    match l1, l2 with
    | [], l2 -> rev_append l2 accu
    | l1, [] -> rev_append l1 accu
    | h1::t1, h2::t2 ->
        let c = cmp h1 h2 in
        if c = 0 then rev_merge t1 t2 (h1::accu)
        else if c < 0
        then rev_merge t1 l2 (h1::accu)
        else rev_merge l1 t2 (h2::accu)
  in
  let rec rev_merge_rev l1 l2 accu =
    match l1, l2 with
    | [], l2 -> rev_append l2 accu
    | l1, [] -> rev_append l1 accu
    | h1::t1, h2::t2 ->
        let c = cmp h1 h2 in
        if c = 0 then rev_merge_rev t1 t2 (h1::accu)
        else if c > 0
        then rev_merge_rev t1 l2 (h1::accu)
        else rev_merge_rev l1 t2 (h2::accu)
  in
  let rec sort n l =
    match n, l with
    | 2, x1 :: x2 :: tl ->
        let s =
          let c = cmp x1 x2 in
          if c = 0 then [x1] else if c < 0 then [x1; x2] else [x2; x1]
        in
        (s, tl)
    | 3, x1 :: x2 :: x3 :: tl ->
        let s =
          let c = cmp x1 x2 in
          if c = 0 then
            let c = cmp x2 x3 in
            if c = 0 then [x2] else if c < 0 then [x2; x3] else [x3; x2]
          else if c < 0 then
            let c = cmp x2 x3 in
            if c = 0 then [x1; x2]
            else if c < 0 then [x1; x2; x3]
            else
              let c = cmp x1 x3 in
              if c = 0 then [x1; x2]
              else if c < 0 then [x1; x3; x2]
              else [x3; x1; x2]
          else
            let c = cmp x1 x3 in
            if c = 0 then [x2; x1]
            else if c < 0 then [x2; x1; x3]
            else
              let c = cmp x2 x3 in
              if c = 0 then [x2; x1]
              else if c < 0 then [x2; x3; x1]
              else [x3; x2; x1]
        in
        (s, tl)
    | n, l ->
        let n1 = n asr 1 in
        let n2 = n - n1 in
        let s1, l2 = rev_sort n1 l in
        Printf.printf "in sort, after rev_sort, s1:";
        print_int_list s1;
        let s2, tl = rev_sort n2 l2 in
        Printf.printf "in sort, after rev_sort, s2:";
        print_int_list s2;
        let rev_merge_rev_res = rev_merge_rev s1 s2 [] in
        Printf.printf "in sort, after rev_merg_rev:";
        print_int_list rev_merge_rev_res;
        (rev_merge_rev s1 s2 [], tl)
  and rev_sort n l =
    match n, l with
    | 2, x1 :: x2 :: tl ->
        let s =
          let c = cmp x1 x2 in
          if c = 0 then [x1] else if c > 0 then [x1; x2] else [x2; x1]
        in
        (s, tl)
    | 3, x1 :: x2 :: x3 :: tl ->
        let s =
          let c = cmp x1 x2 in
          if c = 0 then
            let c = cmp x2 x3 in
            if c = 0 then [x2] else if c > 0 then [x2; x3] else [x3; x2]
          else if c > 0 then
            let c = cmp x2 x3 in
            if c = 0 then [x1; x2]
            else if c > 0 then [x1; x2; x3]
            else
              let c = cmp x1 x3 in
              if c = 0 then [x1; x2]
              else if c > 0 then [x1; x3; x2]
              else [x3; x1; x2]
          else
            let c = cmp x1 x3 in
            if c = 0 then [x2; x1]
            else if c > 0 then [x2; x1; x3]
            else
              let c = cmp x2 x3 in
              if c = 0 then [x2; x1]
              else if c > 0 then [x2; x3; x1]
              else [x3; x2; x1]
        in
        (s, tl)
    | n, l ->
        let n1 = n asr 1 in
        let n2 = n - n1 in
        let s1, l2 = sort n1 l in
        Printf.printf "in rev_sort, after sort, s1:";
        print_int_list s1;
        let s2, tl = sort n2 l2 in
        Printf.printf "in rev_sort, after sort, s2:";
        print_int_list s2;
        let rev_merge_res = rev_merge s1 s2 [] in
        Printf.printf "in rev_sort, after rev_merg:";
        print_int_list rev_merge_res;
        (rev_merge s1 s2 [], tl)
  in
  let len = length l in
  if len < 2 then l else fst (sort len l)

let test_case = [4; 4; 2; 4; 5; 6; 1; 3; 2; 3; 5; 4]
let lst1 = [1; 1; 2; 2; 2; 3; 3; 3]
let lst2 = [1; 2; 2; 3; 3; 4; 5]
let lst3 = [5; 4; 3; 2; 1]
let lst4 = [8; 7; 6; 4; 3]

let _res = sort_uniq Stdlib.compare test_case

let () = 
  let lst1_str = string_of_lst string_of_int lst1 in
  let lst2_str = string_of_lst string_of_int lst2 in
  let lst3_str = string_of_lst string_of_int lst3 in
  let lst4_str = string_of_lst string_of_int lst4 in
  Printf.printf 
    "\n\nrev_merge \nincr lst1: %s\nincr lst2: %s\n" 
    lst1_str lst2_str;
  let reslst1 = rev_merge Stdlib.compare lst1 lst2 [] in
  print_int_list reslst1;
  Printf.printf 
    "rev_merge_rev \nincr lst3: %s\nincr lst4: %s\n" 
    lst3_str lst4_str;
  let reslst2 = rev_merge_rev Stdlib.compare lst3 lst4 [] in
  print_int_list reslst2;