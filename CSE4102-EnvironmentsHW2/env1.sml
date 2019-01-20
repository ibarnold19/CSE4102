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
(* This is a set of tuples in a list that *)
(* are each composed of a string and integer. *)
(* Need to use this to allow environments to be created. *)
type Env = (string * int) list;

(* Function to create a new environment data structure. *)
(* This creates a new environment data strcuture that is an empty *)
(* list.*)
(* Need to use this to allow environments to have new bindings on *)
(* them.*)
fun env_new () : Env = nil;

(* Function to assign a string and int tuple to a new environment. *)
(* This adds on a new tuple onto the environment list so that .*)
(* a string and int can be associated with each other and fetched *)
(* Use this to add on tuples to an existing environment *)
fun env_bind (e : Env) s v : Env = (s, v)::e;

(* Function to search and return an integer value given it's *)
(* corresponding string associated with it in a given environment. *)
(* If the string doesn't exist within the environment, raise *)
(* the NameNotBound exception indicating it isn't in the envrionment *)
(* Use this to find a value of a corresponding string in an *)
(* environment, or to check if one exists and display accordingly. *)
fun env_lookup ([] : Env) s = raise NameNotBound s
  | env_lookup (e : Env) s = if #1(hd e) = s then #2(hd e) else env_lookup ((tl e) : Env) s;