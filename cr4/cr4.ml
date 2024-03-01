open Stack ;;

open CS51Utils.Absbook ;;



(* Inverting a Stack

Consider the polymorphic Stack module as shown in Listing 6 (in the handout). 

1) Define a function invert_stack : 'a Stack.stack -> 'a Stack.stack that inverts the elements of the stack. 

*)



(* This doesn't work because 'a stack is hidden behind the abstraction barrier that the module interface provides *)

let rec rev (lst : 'a list) : 'a list = 
  match lst with 
  | [] -> [] 
  | h :: t -> (rev t) @ [h] ;;

let rec invert_stack (s : 'a stack) : 'a stack = 
  if s = empty then empty else 
  let elt = top s in 
  let rest = pop s in 
  push elt (invert_stack rest) ;;

let rec invert_stack (s : 'a stack) : 'a stack = 
  let rec aux (s : 'a stack) (acc : 'a stack) : 'a stack = 
    if s = empty then acc else 
    let elt = top s in 
    let rest = pop s in 
    invert_stack rest (push elt acc) 
  in aux s empty ;;


(* 
stack = [1; 5; 2; 3] acc = [] 


acc = [1] 

stack = [5; 2; 3] 


acc = [5; 1] 

acc = [3; 2; 5; 1]


invert_stack* [1; 5; 2; 3] = [3; 2; 5; 1] 

invert_stack [1; 5; 2; 3]

elt = 1; rest = [5; 2; 3] 

invert_stack [5; 2; 3] -> [3; 2; 5] 

push 1 [3; 2; 5] -> [1; 3; 2; 5]





*)


(*

2) Write a few unit tests outside the Stack module testing the functionality of invert_stack. 

*)

(* Stack to a list *)
let rec stack_to_list (s : 'a stack) : 'a list = 
  if s = empty then []
  else 
  let elt = top s in 
  let rest = pop s in 
  elt :: stack_to_list rest ;; 




let stack1 = empty |> push 5 |> push 4 |> push 3 ;;

(* let x = e1 in e2 

pop empty

unit_test : bool 

pop empty  : 'a stack  

let x = 5 in false -> false 

let x = 2 in x + 2 ;;


*)
let invert_stack_tests () = 
  unit_test (invert_stack empty = empty) "invert_stack empty"; 
  unit_test (stack_to_list (invert_stack stack1) = [5; 4; 3]) "invert_stack len of 3"; 
  unit_test (top stack1 = 3) "test top";
  unit_test (stack_to_list (pop stack1) = [4; 5]) "test pop"; 
  unit_test (try   
                let _ = pop empty in false 
              with 
                | EmptyStack -> true 
                | _ -> false) "pop empty stack"

  ;;





(*.................

Set Module 

In this problem, we will create a module for sets (slightly different instructions from problem on 
handout)

Sets are unordered lists without any duplicate elements
................*) 

(* 

1) 

Create a module signature for a polymorphic Set that includes the following values and operations on sets are supportable. 

(a) empty : the empty set

(b) add : add an element to a set

(c) take : a function that takes an element in the set and returns the rest of the elements in
the set as a pair : (h, t), where h is the extract element and t are the rest of the elements.

(d) mem : check if an element is a member of a set

(e) union : return the union of two sets
(f) intersection : return the intersection of two sets

(g) print_set : convert a set into a string.

*)


module type SET = 
  sig 
    exception EmptySet 
    type 'a set 
    val empty : 'a set 
    val add : 'a -> 'a set -> 'a set
    val take: 'a set -> 'a * 'a set 
    val mem : 'a -> 'a set -> bool 
    val union : 'a set -> 'a set -> 'a set 
    val intersection : 'a set -> 'a set -> 'a set
    val print_set : 'a set -> ('a -> string) -> string 

  end 


  (* 
  
  add 5 {5; 2; 3} -> {5; 2; 3}
  
  *)

(* 

2) Implement a Set Module of that satisfies the SET interface

*)

module Set : SET = 
  struct 

    exception EmptySet 

    type 'a set = 'a list 


    let empty : 'a set = [] 

    let mem : 'a -> 'a set -> bool = 
      List.mem 

    let add (e : 'a) (s : 'a set) : 'a set = 
      if mem e s then s 
      else e :: s 

    let take (s : 'a set) : 'a * 'a set = 
      match s with 
      | [] -> raise EmptySet 
      | h :: t -> h, t
    
      

    let union (s1 : 'a set) (s2 : 'a set) : 'a set = 
      List.fold_right add s2 s1 

    (* let rec intersection (s1 : 'a set) (s2 : 'a set) : 'a set = 
      match s2 with 
      | [] -> [] 
      | h :: t -> 
        if List.mem h s1 then h :: intersection s1 t 
        else intersection s1 t  *)
(* 
     let intersection (s1 : 'a set) (s2 : 'a set) : 'a set = 
      List.fold_left (fun acc e -> if List.mem e s2 then add e acc else acc) empty s1  *)

    let intersection (s1 : 'a set) (s2 : 'a set) : 'a set = 
      List.filter  (fun x -> mem x s1)  s2

    let print_set (s : 'a set) (serialize : 'a -> string) : string = 
      let rec aux (str : 'a set) : string = 
        match str with 
        | [] -> "" 
        | [e] -> serialize e
        | h :: t -> serialize h ^ "; " ^ aux t 
      in "{" ^ aux s ^ "}"
        
  end  




(* 

3) OUTSIDE the Set module, define three higher order functions that operate over 
sets; map, fold, and filter
 
*)

open Set ;;

let rec map (f : 'a -> 'b) (s : 'a Set.set) : 'b set = 
  if s = empty then empty else
  let h, t = take s in 
  s |> map f |> add (f h) 


let rec fold (f : 'acc -> 'a -> 'acc) (init : 'acc) (s : 'a set) : 'acc = 
  if s = empty then init else
  let first, rest = take s in 
  fold f (f init first) rest  
  
(* f : (+) 

{1, 2, 3, 5} 

init = 0 

1, {2, 3, 5} 

fold f 1 {2, 3, 5}  = 11


*)

 

let rec filter (pred : 'a -> bool) (s : 'a set) : 'a set = 
  if s = empty then empty else 
  let h, t = take s in 
  if pred h then add h (filter pred t) 
  else filter pred t 





(* 

Define a function power_set that returns a set of all the subsets of the original set. For this
problem, you can create a function that returns the power set for a set of integers. For instance,
the power set of {1, 2, 3, 4} is

{{}, {1}, {2}, {3}, {4}, {1, 2}, {1, 3}, {1, 4}, {2, 3}, {2, 4},
{3, 4}, {1, 2, 3}, {1, 3, 4}, {1, 2, 4}, {2, 3, 4}, {1, 2, 3,
4}}
*)

let rec add_el (e : int) (set_set : 'a Set.set Set.set) : 'a Set.set Set.set = 
  if set_set = Set.empty then 
  Set.empty |> Set.add (Set.add e Set.empty) 
  else 
  let first, rest = Set.take set_set in 
  add_el e rest |> Set.add (Set.add e first) 

let rec power_set (s : 'a Set.set) : 'a Set.set Set.set = 
  if s = Set.empty then Set.add (Set.empty) (Set.empty) 
  else 
  let h, t = Set.take s in 
  let power_t = power_set t in 
  Set.union power_t (add_el h (power_t))

