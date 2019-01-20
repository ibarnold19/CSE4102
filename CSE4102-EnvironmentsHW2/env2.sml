(* Environments (Homework 2) *)
(* CSE 4102 Project 2, Spring Semester, 2018 *)
(* Bryan Arnold *)
(* 2/11/2018 *)
(* Section: 001 *)
(* Instructor: Jeffrey A. Meunier *)

(* This is the declaration of the NameNotBound exception. *)
(* This exception is raised if the given search for a string *)
(* in the list of environments isn't present. *)
(* Use this the env_lookup function to check for this. *)
exception NameNotBound of string;

(* Type declaration for Environment type. *)
(* This is a map of string to int *)
(* so each environment is composed of a string and integer. *)
(* Need to use this to allow environments to be created. *)
type Env = string -> int;

(* Function to create a new environment data structure. *)
(* This creates a new environment data strcuture that is empty.*)
(* Need to use this to allow environments to have new bindings on them. *)
fun env_new () : Env = fn (x:string) => 0;

(* Function to assign a string and int to a new environment. *)
(* This adds on a new mapping onto a environment so that .*)
(* a string and int can be associated with each other and fetched *)
(* Use this to add values to an individual existing environment *)
fun env_bind env name v : Env = fn n => if n=name then v else (env n);

(* Function to search and return an integer value given it's *)
(* corresponding string associated with it in a given environment. *)
(* If the string doesn't exist within the environment, raise *)
(* the NameNotBound exception indicating it isn't in the envrionment *)
(* Use this to find a value of a corresponding string in an *)
(* environment, or to check if one exists and display accordingly. *)
(* NOT FINISHED *)
fun env_lookup (e : Env) s : int = raise NameNotBound s;