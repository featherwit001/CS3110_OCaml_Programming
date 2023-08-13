let rec sum = function
  | [] -> 1
  | x :: xs -> x + sum xs