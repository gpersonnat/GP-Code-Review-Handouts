

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
 


(* Problem 2 *)


(* Problem 2 *)

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

class polynomial (coefficients : float list) : polynomial_type = 
 object 
  method get_coefficients : float list = coefficients 
  method evaluate (x : float) : float = failwith "not yet implemented" 
  method solve (c : float) : float list option = None 
 end ;; 

