(* 

Problem 1.2 

An (undirected) graph is a defined set of nodes and a set of edges, where
each edge is a pair of different nodes. Read the description in the Code Review 4 
Handout. 

Consider the signatures of the Vertex and Graph module below
*)

module type VERTEX = 
sig 
  type t 
  val compare : t -> t -> int 
end 

module type GRAPH = 
sig 
  type v (* type of vertex *)
  type graph 

  exception VertexAlreadyExists 
  exception VertexDoesNotExist 
  exception EdgeAlreadyExists 

  val empty : graph (* the empty graph *)
  val addVertex : v -> graph -> graph (* add a vertex to a graph *)
  val addEdge : v -> v -> graph -> graph 
  val vertices : graph -> v list (* returns vertices from graph *)
  val neighbors : v -> graph -> v list (* returns a lsit of vertices for a vertex *)
  val graph_size : graph -> int * int (* returns number of vertices and edges in the graph *)
end 


(* 
Define a functor MakeGraph that takes in a module of type VERTEX and returns 
   a module of type GRAPH. What is the signature of this functor? *)

(* 
  To define MakeGraph, start by defining type graph based on the example given for Figure 1 
*)

(* 
There are two main problems with this definition for graph. First, the type allows us to add
the same vertex twice to the neighbors list for any vertex. Second, our list can contain the
same vertex twice: for instance, the following is expressible [(’a’, []); (’a’, [])].
Let’s address the first problem by representing the neighbors as a set, which is an unordered
collection of unique elements. Use the OCaml Set Module.
*) 

(* 
To address the second problem, suppose we instead represent the adjacency list using a hash
map (or hash table). A hash map is a dictionary data structure in which we map unique keys
to values. To do this, we’ll use the OCaml Map Module, taking the keys to be the vertex and
the values to be the neighbors set, as defined in part 3
*)

(* Define addVertex and addEdge *)

(* Complete the implementation of the module by defining vertices, neighbors, and
graph_size *)


(* Problem 1.3 
Using the MakeGraph functor, define a module IntGraph such that the type of the vertices
in the graph are integers.
*)

(* Define a function paths that takes in an integer graph g and two vertices a and b, and returns
a list of integer lists, such that each list represents an acyclic path from a to b. See 
Code Review handout for example *)


(* 
Problem 1.4 (Set). Let’s implement the Set module from scratch. Sets are an unordered list of
elements with no duplicates

1. Define a module signature for an ORDERED_TYPE which specifies the type of the elements, a
function to compare elements, and a function to convert each of the elements into strings
*)


(*
2. Create a module type for Set that includes the following values and operations on sets are
supportable.
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
3. Write a functor MakeSet that takes in a module of ORDERED_TYPE and returns a module of
type SET
*)



(*
4. Use MakeSet to define the following modules:
(a) A set of integers
(b) A set of strings
(c) A set of int lists
(d) A set of integer sets
*)


(*
5. Define a function power_set that returns a set of all the subsets of the original set. For this
problem, you can create a function that returns the power set for a set of integers. For instance,
the power set of {1, 2, 3, 4} is

{{}, {1}, {2}, {3}, {4}, {1, 2}, {1, 3}, {1, 4}, {2, 3}, {2, 4},
{3, 4}, {1, 2, 3}, {1, 3, 4}, {1, 2, 4}, {2, 3, 4}, {1, 2, 3,
4}}
*)