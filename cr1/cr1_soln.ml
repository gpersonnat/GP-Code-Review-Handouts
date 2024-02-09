

(* Define an infix operator +/. that computes the average of two-floating point 
   numbers *)

let (+/.) (x : float) (y : float) : float =
  (x +. y) /. 2. ;;


(* Define a function fib such that fib n is the nth number in the Fibonacci
sequence, which is 1, 1, 2, 3, 5, 8, 13... *)

let rec fib (n : int) = 
  if n = 1 || n = 2 then 1 
  else fib (n - 1) + fib (n - 2) ;; 


(* Define a function subset_sum that checks if there exists a subset of a
list with a given sum (called the target). *)
let rec subset_sum (target: int) (lst : int list)  = 
  match lst with 
  | [] -> target == 0 
  | h :: tl -> subset_sum target tl || subset_sum (target - h) tl ;; 


(* Define a function partition that divides a list into two lists, such that the
first list contains elements that satisfy a predicate function f and the second list contains elements
that don’t satisfy the predicate. You may assume the input list is an integer list. *)
let rec partition (pred : int -> bool) (lst : int list) : int list * int list =
  match lst with
  | [] -> [], []
  | h :: t ->
    let y, n = partition pred t in
    if pred h then h :: y, n
    else y, h :: n ;;


(* Define a function pack that packs consecutive duplicates of list elements into sublists. 
   You may assume that the input lists are string lists. *)
let rec pack (lst : string list) : string list list =
  match lst with
  | [] -> []
  | h :: t ->
    match pack t with
    | [] -> [[h]]
    | hd :: tl as l ->
      match hd with
      | [] -> []
      | h1 :: _ ->
        if h1 = h then (h :: hd) :: tl
        else [h] :: l ;;


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
type position = char * int ;; 


(* After defining position, define a function count_position that takes in a position and
a position list, and counts the number of times the position occurs in the position
list. *)

let rec count_position (pos : position) (lst : position list) : int =
  match lst with
  | [] -> 0
  | h :: t -> (if h = pos then 1 else 0) + count_position pos t ;;


(* A knight in chess can move two squares vertically and one square horizontally, or two squares
horizontally and one square vertically (in an "L" shape). Define a function legal_knight_moves
that takes in a position and returns a position list representing all the possible posi-
tions the knight can move to. Hint: You may find the Char module helpful here. *)
let legal_knight_moves ((c, r) : position) : position list =
  let possible_moves = 
    [(2, 1); (2, -1); (-2, 1); (-2, -1); (1,2); (-1, 2); (-1, -2); (1, -2)] in 
  let in_bounds ((c, i) : position) : bool = 
    c >= 'a' && c <= 'h' && i >= 1 && i <= 8 in 
  let rec aux (lst: (int * int) list) : position list = 
    match lst with 
    | [] -> [] 
    | (h, v) :: t -> 
      let pros_move = (Char.chr (Char.code c + h), r + v) in 
      if in_bounds pros_move then pros_move :: aux t else aux t 
    in aux possible_moves ;; 
   


(* Define a function min_knight_steps that takes in a starting position s and a target
position t, and returns the minimum number of moves it takes for a knight to go from s or 6
t. Note, that a knight can visit any position on the board from any starting position, so there’s
always a solution! See the Knight’s tour. You may use the List.map function here. *)
let rec min_knight_steps (s : position) (t : position) : int =
  let rec min_list (lst : int list) : int = 
    match lst with 
    | [] -> -1 
    | [elt] -> elt 
    | h :: t -> min h (min_list t) in 
  if s = t then 0 
  else 1 + min_list (List.map (fun x -> min_knight_steps x t) (legal_knight_moves s)) ;; 