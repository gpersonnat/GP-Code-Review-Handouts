

(* 


Tail recursive function - a function whose last computation ends in a recursive call 
*)

let rec length (lst : 'a list) : int = 
  match lst with 
  | [] -> 0 
  | _ :: t -> 1 + length t ;;

(* Non-recursive because last computation isn't recursive call *)


(* 'a list -> int 
'a list -> int -> int
*)
let length (lst : 'a list) : int = 
  let rec length_inner (l : 'a list) (acc : int) : int = 
    match l with 
    | [] -> acc 
    | _ :: t -> length_inner t (acc + 1) 
  in length_inner lst 0 ;;

(* Tail recursive 



*)