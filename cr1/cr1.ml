

(* Define an infix operator +/. that computes the average of two-floating point 
   numbers *)

let (+/.) (x : float) (y : float) : float = 
  (x +. y) /. 2. ;; 


(* Define a function fib such that fib n is the nth number in the Fibonacci
sequence, which is 1, 1, 2, 3, 5, 8, 13... *)

let rec fib (n : int) : int = 
   if n = 1 || n = 2 then 1 
   else fib (n - 1) + fib (n - 2)  ;; 


(* Define a function subset_sum that checks if there exists a subset of a
list with a given sum (called the target). *)
let rec subset_sum (lst : int list) (target : int) : bool = 
   match lst with 
   | [] -> target = 0
   | h :: t -> 
      subset_sum t (target - h) || subset_sum t target ;; 
(* 

Thinking Recursively 

1) We define base cases -> inputs that you can solve directly 


2) Recursive case -> you make recursive calls to your function 

Assume that recursive calls on smaller cases are correct 

Exhaustive Cases : 

1) Don't include first element in the subset -> subset_sum t target 

2) Include the head element in the subset -> subset_sum t (target - h) 





*)


(* Define a function partition that divides a list into two lists, such that the
first list contains elements that satisfy a predicate function f and the second list contains elements
that don’t satisfy the predicate. You may assume the input list is an integer list. *)

let partition_filter (pred : int -> bool) (lst : int list) : int list * int list = 
   List.filter pred lst, List.filter (fun x -> not (pred x)) lst ;; 

let rec partition (pred : int -> bool) (lst : int list) : int list * int list =
   match lst with 
   | [] -> [], []
   | h :: t -> 
      let x, y = partition pred t in 
      if pred h then h :: x, y else x, h :: y 
;; 

(* 

1) Base Case -> sovle the empty list directly 

2) Recursive Case 

(fun x -> x < 3) [4; 7; 2; -3; 5; 9]

partition  f [7; 2; -3; 5; 9] -> ([2; -3], [7; 5; 9])

::  : cons constructor 

2 :: [] = [2] 

2 :: 5 :: 7 :: [1] = [2; 5; 7; 1] 

*)


(* Define a function pack that packs consecutive dupli-
cates of list elements into sublists. You may assume that the input lists are string lists. *)
let pack lst = failwith "not yet implemented" ;; 


(* 
Chess:
In chess, we move pieces across a 8x8 board such that the columns are labeled
as letters and the rows are labeled as numbers.

Positions on a chess board are referred to as a combination of the column letter and row number. For
instance, for the white pieces, we can say that the center pawn on the white square is at "e2".

Suppose, we want to define a type position that represents the position of a piece on the
board. What type expression is appropriate to represent a piece’s position? (note that int is just used 
a placeholder below so that this will compile)

*)
type position = int ;; 


(* After defining position, define a function count_position that takes in a position and
a position list, and counts the number of times the position occurs in the position
list. *)

let count_position pos lst = failwith "not yet implemented" ;; 


(* A knight in chess can move two squares vertically and one square horizontally, or two squares
horizontally and one square vertically (in an "L" shape). Define a function legal_knight_moves
that takes in a position and returns a position list representing all the possible posi-
tions the knight can move to. Hint: You may find the Char module helpful here. *)
let legal_knight_moves position = failwith "not yet implemented" ;; 


(* Define a function min_knight_steps that takes in a starting position s and a target
position t, and returns the minimum number of moves it takes for a knight to go from s or 6
t. Note, that a knight can visit any position on the board from any starting position, so there’s
always a solution! See the Knight’s tour. You may use the List.map function here. *)
let min_knight_steps s t = failwith "not yet implemented" ;; 