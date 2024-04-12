

(* Problem 1 


1) Write a class definition for a class loud_counter obeying the same interface that works
identically, except that it also prints the resulting state of the counter each time the
counter is bumped.

2) Write a class type definition for an interface reset_counter_interface, which is
just like counter_interface except that it has an additional method of no arguments
intended to reset the state back to zero.

3) Write a class definition for a class loud_reset_counter satisfying the reset_counter_-
interface that implements a counter that both allows for resetting and is “loud”
(printing the state whenever a bump or reset occurs).

*)


class type counter_interface =
   object
      method bump : int -> unit
      method get_state : int
   end ;;

class counter : counter_interface =
   object
      val mutable state = 0
      method bump n = state <- state + n
      method get_state = state
   end ;;

class loud_counter : counter_interface = 
   object (this) 
      inherit counter as super
      
      method! bump (n : int) : unit = 
         super#bump n;
         Printf.printf "State is now %d\n" this#get_state
   end

class type reset_counter_interface = 
   object 
      inherit counter_interface 
      method reset : unit 
   end

let ctr1 : counter_interface = new loud_counter ;;

let loud_reset_counter : reset_counter_interface = 
   object (this)
      inherit loud_counter as super

      method reset : unit = 
         this#bump ((~-)this#get_state) 
   end


(* Problem 2 

1. Define a class interface for a user user that includes the following methods:
(a) Get a user’s username
(b) Get a user’s student id (a string)
(c) Add a friend to a user’s list of friends; note that friendships are mutual, so if Alice is a
friend of Bob, then Bob is a friend of Alice.
(d) Get a user’s friend list
(e) Add a post (a string) to a user’s list of posts
(f) Remove a post
(g) Retrieve all the user’s posts

*)


class type user_type = 
   object 
      method get_username : string 
      method get_id : string 
      method add_friend : user_type -> unit 
      method get_friends : user_type list 
      method add_post : string -> unit 
      method remove_post : string -> unit 
      method get_posts : string list 
   end

(* 
Define an implementation that satisfies the class interface which takes in a username and an
id.
*)

class user (username : string) (id : string) : user_type = 
   object (this)

   val mutable friends : user_type list = []
   val mutable posts : string list = [] 

   method get_username : string = username 

   method get_id : string = id 

   method add_friend (friend : user_type) : unit = 
      if List.mem friend friends then 
      () 
      else 
      (friends <- friend :: friends; friend#add_friend (this :> user_type)) 

   method get_friends : user_type list = friends 

   method add_post (post : string) : unit = 
      posts <- post :: posts 

   method remove_post (post : string) : unit = posts <- List.filter ((!=) post) posts 

   method get_posts : string list = posts



   end


  (* 3. Define a class interface called student which has the same functionality as the user class
   but an additional method which outputs the school a student attends. *)

class type student_interface = 
   object 
      inherit user_type 
      method get_school : string
   end

(* 

4. Define an implementation that satisfies the student interface which takes in a username,
id, and the school the student attends.
*)

class student (username : string) (id : string) (school : string) : student_interface = 
   object 
      inherit user username id 

      method get_school : string = school
   end

(* 

5. Define a function form_friend_group that takes in a list of users, and for each user in the
list, sets their friends to be everyone else in the list.

*)

let rec form_friend_group (lst : user_type list) : unit = 
   match lst with 
   | [] -> () 
   | h :: t -> 
      (* Iterate through the rest of the users and add head to that user's friends list 
      List.fold_left (add's a friend to user's list) () lst 
      List.iter
      [G; J; A; B] 

      add  J, A, and B to G's list of friends 

      # List.iter on the [J; A; B] (fun user -> G#add_friend user)




      *)
      List.iter (fun user -> h#add_friend user) t;
      form_friend_group t ;;

(* 

6. Define a list of four students and use form_friend_group on the student list.
*)

let jayden : student_interface = new student "jaydenp" "1" "Harvard" ;; 

let gerson : student_interface= new student "gersonp" "2" "Harvard" ;;

let kwee : student_interface = new student "kwee" "3" "Harvard" ;;

let victoria : student_interface = new student "victoriol" "4" "Harvard" ;;

let student_list : student_interface list = [jayden; gerson; kwee; victoria] ;;

(* student_interface list is a suptype of a user_type list  *)

(* expr :> type 

It says the type of expr is a subtype of type

*)

form_friend_group (student_list :> user_type list)

(* Problem 3 *)

class type polynomial_type = 
 object 

  (* returns the coefficients of the polynomial in order from lowest degree to highest
     degree *)
  method get_coefficients : float list 
  
  (* `evaluate` x evalutes a polynomial f at x, i.e. computes f(x) *)
  method evaluate : float -> float 

  (* solve c returns real solutions for x when a polynomial equals c. In other words, 
     it should return the real solution solutions x such that f(x) = c *)
  method solve : float -> float list option 

 end ;; 

 (* 
 
 a_0 + a_1x^1 + a_2x^2 + ... + a^nx^n


 [5; 3; 2] 

 5 + 3x + 2x^2

 to_int - Pset3 
 
 *)

class polynomial (coefficients : float list) : polynomial_type = 
 object 
  method get_coefficients : float list = coefficients 
  method evaluate (x : float) : float = 
      (* similar to_int from Pset3 *)
      List.fold_right (fun coeff acc -> x *. acc +. coeff) coefficients 0.0 
  method solve (c : float) : float list option = None 
 end ;; 


(* 
. Implement a linear_polynomial class satisfying the polynomial_type class interface.
A linear polynomial is an expression that has at most two coefficients. For example, 5x + 2
(with coefficients as [2.0, 5.0]), 3x ([0., 3.]), and 11 ([11.]) are all polynomial functions. Hint: You
may define a type so that users can only define at most two coefficients.
For a constant polynomial c, solve a should return [Float.infinity] if c = a, None
otherwise.
*)

(* edict of prevention - we only want someone to define a linear function with at most two coefficients *)

type linear_coefficients = 
   | Constant of float 
   | Linear of float * float 

let rec extract_linear_coeffs (coeffs : linear_coefficients) = 
   match coeffs with 
   | Constant c -> [c] 
   | Linear (intercept, slope) -> 
      if slope = 0. then extract_linear_coeffs (Constant intercept) 
      else [intercept; slope] ;;

class linear_polynomial (coefficients : linear_coefficients) : polynomial_type = 
   object (this)
   inherit polynomial (extract_linear_coeffs coefficients) 

   (*  mx + b = c 
   
   x = (c - b) / m

   a = c -> infinitely many solution 

   a != c -> no solutions
   
   *)
   method! solve (c : float) : float list option = 
      match this#get_coefficients with 
      | [constant] -> if c = constant then Some [Float.infinity] else None 
      | [intercept; slope] -> 
         Some [( c -. intercept ) /. slope]
      | _ -> failwith "solve : Invalid linear polynomial"

   end

type quadratic_coeffs = 
   | Constant of float 
   | Linear of float * float 
   | Quadratic of float * float * float (* ax^2 + x + c *)

let rec extract_quad_coeffs (coeffs : quadratic_coeffs) : float list = 
   match coeffs with 
   | Constant c -> [c] 
   | Linear (intercept, slope) -> 
      if slope = 0. then  extract_quad_coeffs (Constant intercept) else [intercept; slope] 
   | Quadratic (c, b, a) -> 
      if a = 0. then extract_quad_coeffs (Linear (b, c))
      else [c; b; a]

(* class quadratic_polynomial (coefficients : quadratic_coeffs) : polynomial  =
   object (this)
   inherit polynomial (extract_quad_coeffs coefficients) 
   method !solve (c : float) : float list option = 
      match this#get_coefficients with
      | [c_prime; b; a] -> 
         if inner = b ** 2. (* Follow quadratic formula  *)


   end *)
