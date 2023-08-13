(* chapter 2 *)
(* fast fib *)
let rec fib = function
  | 1 | 2 -> 1
  | n -> fib(n - 1) + fib(n - 2)

(* [h n p pp] is the fibonacci number with linear time complexity
  [n] is the ordinal number. 
  [p] and [pp] is the (n)th and (n - 1)th fabonacci numbers when n > 2 *)
let rec h n p pp = 
  match n with
  | 1 -> p
  | n when n > 1 -> begin
      h (n - 1) (p + pp) p
    end
  | _ -> failwith "h is undefined on nagetive ints"
let fast_fib n = h n 1 0


let fib_lst n = List.init n (fun n -> fib (n + 1)) 

let fast_fib_lst n = List.init n (fun n -> fast_fib(n + 1))

(* What is the first value of n for which fib_fast n is negative, 
   indicating that integer overflow occurred? 
   102 *)