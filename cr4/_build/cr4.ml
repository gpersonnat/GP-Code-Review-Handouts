open Stack ;;

open CS51Utils.Absbook ;;



(* Inverting a Stack

Consider the polymorphic Stack module as shown in Listing 6 (in the handout). 

1) Define a function invert_stack : 'a Stack.stack -> 'a Stack.stack that inverts the elements of the stack. 

*)

let invert_stack (s : 'a stack) : 'a stack = 
  let rec aux (s : 'a stack) (acc : 'a stack) : 'a stack =
    if s = empty then acc else 
    let h = top s in 
    let t = pop s in 
    aux t (push h acc) in 
  aux s empty ;;


(*

2) Write a few unit tests outside the Stack module testing the functionality of invert_stack. 

*)




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



(* 

2) Implement a Set Module of that satisfies the SET interface

*)



(* 

3) OUTSIDE the Set module, define three higher order functions that operate over 
sets; map, fold, and filter
 
*)





(* 

Define a function power_set that returns a set of all the subsets of the original set. For this
problem, you can create a function that returns the power set for a set of integers. For instance,
the power set of {1, 2, 3, 4} is

{{}, {1}, {2}, {3}, {4}, {1, 2}, {1, 3}, {1, 4}, {2, 3}, {2, 4},
{3, 4}, {1, 2, 3}, {1, 3, 4}, {1, 2, 4}, {2, 3, 4}, {1, 2, 3,
4}}



*)



