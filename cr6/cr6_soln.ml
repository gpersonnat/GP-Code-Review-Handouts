
(* 
Define a function mem_factorial which takes a unit as an argument and returns 1 the first
time it is called, returns 2 the second time it is called, returns 6 the third time it is called, and
so on. In general, the function should return n! on the nth time it is called. 
*)

let mem_factorial = 
  let n = ref 0 in 
  let fact = ref 1 in 
  fun () : int -> 
    let prev = if !n = 0 then 1 else !fact * (!n + 1) in 
    n := !n + 1;
    fact := !n * !fact;
  prev ;;
    

(* Define a function that generates the Fibonacci numbers (i.e. fib(i) the ith time the function
is called *)
let fib = 
  let n = ref 0 in 
  let first = ref 1 in 
  let second = ref 1 in 
  fun () : int -> 
    if 
      !n = 0 || !n = 1 then ((n := succ !n); 1)
    else 
      let num = !first + !second in 
        n := succ !n; 
        first := !second; 
        second := num; 
        num  ;; 


(* Rewrite power from Part 2 (see Problem 6) to improve the worst-case time complexity. 
What is the asymptotic runtime of the function right now? *)
let rec power (x : int) (n : int) : int =
  if 
    n = 0 then 1
  else 
    let n2_power = power x (n / 2) in 
      if n mod 2 = 0 then 
        n2_power * n2_power 
    else 
        n2_power * n2_power * x  ;; 