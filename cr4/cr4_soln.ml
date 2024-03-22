
(* 

Problem 1.1 

Consider the polymorphic Stack module as shown in Listing 6. Note that we've implemented 
this module (as a file) in stack.ml 
   
Define a function invert_stack : 'a Stack.stack -> 'a Stack.stack that inverts 
the elements of a stack 
*)

let invert_stack (s : 'a Stack.stack) : 'a Stack.stack = 
 let open Stack in 
  let rec invert_stack_aux (s : 'a Stack.stack) 
                            (acc : 'a Stack.stack) : 'a Stack.stack = 
    if s = empty then acc 
    else 
      let top_elt = top s in 
      let rest = pop s in 
    invert_stack_aux rest (push top_elt acc) 
  in invert_stack_aux s empty ;; 


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
  val neighbors : v -> graph -> v list (* returns a list of neighbors for a vertex *)
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


module MakeGraph (Vertex : VERTEX) : (GRAPH with type v = Vertex.t) = 
struct
  type v = Vertex.t 

  (* Create a MAP module *)
  module VMap =  Map.Make(
    struct 
      type t = v 
      let compare = Vertex.compare
    end)

  (* Create a Set Module for neigbhors *)
  module VSet = Set.Make(
    struct 
      type t = v 
      let compare = Vertex.compare 
    end)

  (* Specify type of graph as map data structure with values that are sets *)
  type graph = VSet.t VMap.t

  exception VertexAlreadyExists 
  exception VertexDoesNotExist 
  exception EdgeAlreadyExists 

  let empty : VSet.t VMap.t = VMap.empty ;;

  let addVertex (vertex : v) (g : graph) : graph = 
    if VMap.mem vertex g then raise VertexAlreadyExists 
    else VMap.add vertex VSet.empty g ;; 

  let addEdge (s : v) (t : v) (g : graph) : graph = 
    let addEdge_aux (new_set : VSet.t) (old_set : VSet.t option) : VSet.t option = 
      match old_set with 
      | None -> raise VertexDoesNotExist
      | _ -> Some new_set 
    in 
    try 
      let new_neighbors_s = VSet.add t (VMap.find s g) in 
      let new_neighbors_t = VSet.add s (VMap.find t g) in 
      g |> VMap.update s (addEdge_aux new_neighbors_s) 
        |> VMap.update t (addEdge_aux new_neighbors_t) 
    with _ -> raise VertexDoesNotExist ;; 
        
  let vertices (g : graph) : v list  = 
    List.map fst (VMap.bindings g) 
  let neighbors (vertex : v) (g : graph) : v list =
    VSet.elements (VMap.find vertex g) 

  (* construct a set that contains vertex edge pairs *)
  module EdgeSet = Set.Make(
    struct
      type t = v * v 
      let compare ((a, b) : t) (c, d: t) : int = 
        if 
          (Vertex.compare a c = 0  && Vertex.compare b d = 0) 
          || (Vertex.compare a d = 0  && Vertex.compare b c = 0)
        then 0  
        else 1 
    end
  )
  let find_edges (g : graph) : (v * v) list = 
    let open List in 
     let all_potential_edges =  
       fold_left (fun acc (vertex, neighs) -> 
       fold_left (fun edges neigh -> 
          EdgeSet.add (vertex, neigh) edges) acc (VSet.elements neighs)) 
          EdgeSet.empty (VMap.bindings g)
   in EdgeSet.elements all_potential_edges ;; 

  let graph_size (g : graph) = 
    let num_vertices = List.length (vertices g) in 
    let num_edges = List.length (find_edges g) in 
    num_vertices, num_edges ;; 
end

module IntVertex : VERTEX with type t = int = 
  struct 
    type t = int 
    let compare = Stdlib.compare 
  end 

module IntGraph : GRAPH with type v = int =
  MakeGraph(IntVertex) ;; 

  
(* 
Problem 1.4 (Set). Let’s implement the Set module from scratch. Sets are an unordered list of
elements with no duplicates

1. Define a module signature for an ORDERED_TYPE which specifies the type of the elements, a
function to compare elements, and a function to convert each of the elements into strings

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

3. Write a functor MakeSet that takes in a module of ORDERED_TYPE and returns a module of
type SET

4. Use MakeSet to define the following modules:
(a) A set of integers
(b) A set of strings
(c) A set of int lists
(d) A set of integer sets

5. Define a function power_set that returns a set of all the subsets of the original set. For this
problem, you can create a function that returns the power set for a set of integers. For instance,
the power set of {1, 2, 3, 4} is

{{}, {1}, {2}, {3}, {4}, {1, 2}, {1, 3}, {1, 4}, {2, 3}, {2, 4},
{3, 4}, {1, 2, 3}, {1, 3, 4}, {1, 2, 4}, {2, 3, 4}, {1, 2, 3,
4}}
*)

module type ORDERED_TYPE = 
  sig 
    type t 
    val compare : t -> t -> int 
    val serialize : t -> string
  end

module type SET = 
  sig 
    exception EmptySet
    type element 
    type set 
    val empty : set 
    val add : element -> set -> set 
    val take : set -> element * set 
    val mem : element -> set -> bool 
    val union : set -> set -> set 
    val intersection : set -> set -> set 
    val print_set : set -> string
  end


 module MakeSet (Element : ORDERED_TYPE) : (SET with type element = Element.t) = 
   struct 
     exception EmptySet
     type element = Element.t 
     type set = element list 
 
     let empty : set = [] 
 
     let add (e : element) (s : set) : set = 
       if List.mem e s then s else e :: s 
 
     let take (s : set) : element * set = 
       match s with 
       | [] -> raise EmptySet 
       | h :: t -> h, t 
 
     let mem : element -> set -> bool = 
       List.mem 
 
     let union (s1 : set) (s2 : set) : set = 
       List.fold_right add s2 s1 
 
      let intersection (s1 : set) (s2 : set) : set = 
         List.filter  (fun x -> mem x s1)  s2
   
       let print_set (s : set) : string = 
         let rec aux (str : set) : string = 
           match str with 
           | [] -> "" 
           | h :: t -> Element.serialize h ^ "; " ^ aux t 
         in "{" ^ aux s ^ "}"
   end
 

 module IntSet : SET with type element = int = 
  MakeSet(
    struct type t = int 
    let compare = 
      Int.compare
    let serialize = 
      string_of_int 
  end)
 let string_of_list (convert : 'a -> string) (lst : 'a list)  : string = 
   let rec aux (str : 'a list) : string = 
     match str with 
     | [] -> "" 
     | h :: t -> convert h ^ "; " ^ aux t 
   in "[]" ^ aux lst ^ "]"
 
 module IntListSet : SET with type element = int list = 
    MakeSet(
      struct 
        type t = int list 
        let compare = List.compare Stdlib.compare 
        let serialize = string_of_list string_of_int
      end)

 module StringSet : SET with type element = string =
   MakeSet(
    struct 
      type t = string 
      let compare = String.compare
      let serialize x = x 
    end)
 module IntSetSet : SET with type element = IntSet.set = 
    MakeSet(
      struct 
        type t = IntSet.set 
        let compare (s1 : IntSet.set) (s2 : IntSet.set) : int = 
          Stdlib.compare s1 s2
        let serialize : IntSet.set -> string = 
          IntSet.print_set 
      end)                             

 
 (* Add an element to a set of sets *)
 let rec add_el (e : int) (set_set : IntSetSet.set) = 
   if set_set = IntSetSet.empty then 
      IntSetSet.empty |> IntSetSet.add (IntSet.add e IntSet.empty)
   else 
    let h, t = IntSetSet.take set_set in 
    add_el e t |> IntSetSet.add (IntSet.add e h) ;; 
  
let rec power_set (s : IntSet.set) : IntSetSet.set = 
  if s = IntSet.empty then IntSetSet.add (IntSet.empty) (IntSetSet.empty) 
  else 
  let h, t = IntSet.take s in 
  let power_t = power_set t in 
  IntSetSet.union power_t (add_el h (power_t)) ;; 
 
 
 
