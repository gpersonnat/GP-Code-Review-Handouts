
(* 

1) Currying 


2) Polymorphism

3) List Abstractions 

4) Options + exceptions

5) Practice Problems 

*)

(* Currying 

What is a curried function? - is a function that takes in its arguments one at a time 

All functions are curried 

Uncurried functions - 

*)

let add = fun x -> fun y -> x + y ;;


(* 

Example: 

Write curry and uncurry functions that convert between curried and uncurried functions 

*)

(* 

Curry -> converts from uncurried to curried function 

1) What is the type of curry? 

Uncurried          Curried
('a * 'b -> 'c) -> ('a -> 'b -> 'c)

*)



let curry (uncurried : ('arg1 * 'arg2) -> 'out) : 'arg1 -> 'arg2 -> 'out = 
  fun x y -> uncurried (x, y)  ;;



let curry (uncurried : ('a * 'b) -> 'c) (x : 'a) (y : 'b) : 'c = 
  uncurried (x, y) ;;

(* 



(('a * b' -> 'c) -> ('a -> 'b -> 'c))


('a * 'b -> 'c) -> 'a -> 'b -> 'c 

-> is right associative 

*)

let uncurry (curried : ('a -> 'b -> 'c)) ((x, y) : 'a * 'b) : 'c = 
  curried x y ;;

(* Polymorphism 

Type variables - variables that represent polymorphic types, 'a, 'b, 'c ; 

Polymorphic types - type expressions that contain type variables 

*)

let id (x : 'arg) : 'arg = x ;;


(*  

List Abstractions 

Abstraction - generalizing concrete details to focus on more important ideas   

1) Map 

2) Filter 

3) Fold 

*)


(* 

Map takes a function f and a list and returns a list with the function applied on every element of the list 

List.map (fun x -> x * x) [2; 5; 6] = [4; 25; 36]

*)

(* 

What is the type of map? 

function list -> list 

map :  ('a -> 'b) -> 'a list -> 'b list 

*)

let rec map (f : 'a -> 'b) (lst : 'a list) : 'b list = 
  match lst with 
  | [] -> [] 
  | h :: t -> (f h) :: map f t ;;

(* 


Filter - outputs the elements in a list that satisfy a predicate function

filter : ('a -> bool) -> 'a list -> 'a list

let evens (lst : int list) : int list = 
  List.filter (fun n -> n mod 2 = 0) lst ;; 

Partial application 

let evens : int list -> int list = 
  List.filter (fun n -> n mod 2 = 0) ;;

*)

let rec filter (pred : 'a -> bool) (lst : 'a list) : 'a list = 
  match lst with 
  | [] -> [] 
  | h :: t -> 
    let filter_tl = List.filter pred t in 
    if pred h then h :: filter_tl 
    else filter_tl ;;

(* 

Fold : take in accumulator, go through a list, and perform a function that takes in the accumulator and an element in the list 
and updates the accumulator 

function -> inital value of the accumulator -> lst -> accumulator 

fold_left : ('acc -> 'elem -> 'acc) -> 'acc -> 'elem list -> 'acc

List.fold_left (+) 0 [2; 5; 7] = 14 ;;

1) acc = 0 

2) acc = 2 

3) acc = 7 

4) acc = 14 




fold_right : ('elem -> 'acc -> 'acc) -> 'elem list -> 'acc -> 'acc 

List.fold_right (+) [2; 5; 7] 0 = 14 ;;


*)

(* Fold_left vs fold_right  


1) Fold_left -> goes through elements in the list from left to right 

2) Fold_right -> goes through elements in the list from right to left 

List.fold_left (+) 0 [2; 5; 7] = 14 ;;

acc = 2 + 5 + 7 

fold_left is more space efficent -> uses less stack frames 

fold_left is tail recursive while fold_right isn't 

tail_recusive -> function that ends with a recursive call 

non tail_recursive -> function where the last computation is not a recursive call


List.fold_right (+) [2; 5; 7] 0 = 14 ;;

acc = 7 + 5 + 2 

*)

let rec fold_left (f : 'acc -> 'elem -> 'acc) (init : 'acc) (lst : 'elem list) : 'acc = 
  match lst with 
  | [] -> init 
  | h :: t -> fold_left f (f init h) t ;; 

(*            Last computation is recursive call to fold_left     *)

let rec fold_right (f : 'elem -> 'acc -> 'acc) (lst : 'elem list) (init : 'acc) : 'acc = 
  match lst with 
  | [] -> init 
  | h :: t -> f h (fold_right f t init) ;;

(*              Last computation is applying the function f    *)


(*

Options and Exceptions 

Anonmaly - conditions that a function can't handle 

Ex. You a function that takes the max element in a list -> anomaly : empty list 



*)


(* 
Options 


type 'a option = None | Some of 'a ;;

let ex : int option = None 

let ex : int option = Some 5 ;;


*)

let rec max_list_opt (lst : 'a list) : 'a option = 
  match lst with 
  | [] -> None
  | h :: t -> 
    match max_list_opt t with 
    | None -> Some h 
    | Some max_tail -> Some (max h max_tail) ;;


(* 

Option Poisoning -> extraneous extraction of values inside options 

match (max_list_opt [2;5; 6; 7]) with 
| None -> None 
| Some v -> v + 6 

(max_list_opt [2; 5; 6; 7]) + 6

Options don't really work well when anomalies are rare 

*)

(*  

Exceptions -> errors that stop the function 

exn 

raise : exn -> 'a


*)
let rec max_list (lst : 'a list) : 'a = 
  match lst with 
  | [] -> raise (Invalid_argument "empty list")
  | h :: t -> max h (max_list t) ;;

let div_opt (x : int) (y : int) : int option = 
  if y = 0 then None 
  else Some (x / y) ;;


let evens (lst : int list) : int list = 
  List.filter (fun n -> n mod 2 = 0) lst ;;

let evens : int list -> int list = 
  fun (lst : int list) -> List.filter (fun n -> n mod 2 = 0) lst

let evens : int list -> int list = 
  List.filter (fun n -> n mod 2 = 0) ;;



(*

(+) : int -> int 

fun n -> n * ~-1 

(-~)

*)

(* 

List.filter : ('a -> 'bool) -> 'a list -> 'a list 

List.filter (fun n -> n mod 2 = 0) : int list -> int list 

*)


(* 




*)