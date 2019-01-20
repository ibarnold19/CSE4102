(* Environments (Homework 2), Modified for HW3 *)
(* CSE 4102 Project 2, Spring Semester, 2018 *)
(* Bryan Arnold *)
(* 3/4/2018 *)
(* Section: 001 *)
(* Instructor: Jeffrey A. Meunier *)

(* This is the declaration of the NameNotBound exception. *)
(* This exception is raised if the given search for a string *)
(* in the list of environments isn't present. *)
(* Use this the env_lookup function to check for this. *)
exception NameNotBound;

(* Type declaration for Environment type. *)
(* This is a map of a' to a' *)
(* so each environment is composed of two unknown types. *)
(* Need to use this to allow environments to be created. *)
type 'a Env = string -> 'a;

(* Function to create a new environment data structure. *)
(* This creates a new environment data strcuture that is empty.*)
(* Need to use this to allow environments to have new bindings on them. *)
fun env_new () : 'a Env = fn x => raise NameNotBound;

(* Function to assign two unknown types to a new environment. *)
(* This adds on a new mapping onto an environment so that *)
(* two unknown types can be associated with each other and fetched *)
(* Use this to add values to an individual existing environment *)
fun env_bind env name v : 'a Env = fn n => if n=name then v else (env n);

(* NOT Needed *)
(* fun env_lookup (e : Env) s : int = raise NameNotBound s; *)