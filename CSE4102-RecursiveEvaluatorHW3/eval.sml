(* Bryan Arnold *)
(* CSE 4102 *)
(* HW3: Recursive Evaluator *)
(* 3/4/18 *)

(* These are used to import the functions *)
(* from env.sml, a previous assignment *)
(* with some modifications, as well as *)
(* telling the print out in the console of ML *)
(* to print out 32 lines instead of 4 when something *)
(* is too long *)
use "env.sml";
Control.Print.printDepth := 32;

(* These exceptions are used in the eval function. *)
(* The first one is raised when Add is used incorrectly, *)
(* aka when one expr isn't an integer. *)
(* The next exception is for the condition datatype, *)
(* aka when a bool value isn't given to it. *)
(* The third is for the application datatype, *)
(* aka when the parameters given are not identifiers. *)
(* The last exception is for application datatype as well, *)
(* aka when the inputs given to application are not applied *)
(* to a function expression *)
exception InvalidEvaluation of string;
exception InvalidCondition of string;
exception InvalidParameter of string;
exception InvalidApplication of string;


(***************************************************************************)


(* Expression datatype declaration/definitions. *)

(* Bool expr: this expression will function exactly the same *)
(* as the built in boolean in ML. Use this when you need a *)
(* boolean value in an expression that is to be evaluated. *)

(* Int expr: this expression will function exactly the same *)
(* as the built in integer in ML. Use this when you need an *)
(* integer value in an expression that is to be evaluated. *)

(* Add expr: this expression is to add two Int expressions together. *)
(* Use this to evaluate two Int expressions in an expression. *)
(* If expr: this is how a conditional statement would work in ML *)
(* by using If,...,then,...else,... Use this if you need a conditional *)
(* in the expression to be evaluated. *)

(*Let expr: this expression will function the same as *)
(* the let statement in ML would be. It takes a list of *)
(* string expression tuples as well as an expression as the body *)
(* use this like you would the let in ML *)

(* Def expr: this functions to allow names to be defined *)
(* in the environment. This works just like val in ML. *)
(* Use this if you need a name to be declared for an expression. *)

(* Fun expr: this expression is how an anonymous functions would *)
(* work in ML, fn x => y. Use this to define a functions just like in ML. *)

(* App expr: this expression is how one would call a function expression *)
(* previously described. Use this to call some Fun expr declared already *)

(* Seq expr: this is a list of expressions to be *)
(* evaluated. This is just like the parenthetical operator in *)
(* ML. Use this to create a sequence of expressions to be evaluated. *)

(*Disp expr: this is just a way to display any expression declared. *)
(* Use this just like the built-in display operator to display some expression. *)

(* Closure expr: this contains the environment where a *)
(* function is defined. Use this to save an environment with a *)
(* function. *)

(*Nothing expression: this is just a value to denote the null value *)
(* in ML, (). Use this to return nothing if needed. *)

datatype Expr = Bool of bool 
              | Int of int 
              | Add of Expr * Expr 
              | If of Expr * Expr * Expr 
              | Ident of string 
              | Let of (string * Expr) list * Expr 
              | Def of string * Expr 
              | Fun of Expr * Expr 
              | App of Expr * Expr 
              | Seq of Expr list 
              | Disp of Expr 
              | Closure of Expr * Expr * Expr Env 
              | Nothing;


(****************************************************************************************************)

(* e2s function for each expression type. *)
(* This function is responsible for taking an expression and converting it *)
(* into its string equivalent. This function can be variable on how each string is *)
(* displayed. I went with the ML constructor syntax for each expression. Use this function *)
(* to be able to properly display the expression being evaluated. *)

(* The Bool expression simply uses the toString in ML to *)
(* change the boolean expression to its string equivalent. *)

(* The Int expression simply uses the toString in ML to *)
(* change the integer expression to its string equivalent. *)

(* The Add expression displays its expression as Add(x ,y). It *)
(* displays the add as normal, while calling e2s on the two variables *)
(* that make up the add expression (2 Int expressions). *)

(* The If expression will display just as a normal conditional statement. It *)
(* won't display if,..,then,...,else, but a comma seperated list of the components *)
(* of the conditional. So, expr1, expr2, expr2. *)

(* Ident expression is only a string, so displaying it will just display the string *)
(* associated with the expression. *)

(* Let expression utilizes a helper function to display, since it is a *)
(* list with an expression. It fully displays the expression string tuple list, *)
(* followed by the expression body. *)

(* Def expression is also very simple like Ident. It just has an expression string tuple, *)
(* so it displays the string, expression. *)

(* Fun expression is also simple since it is a expression expression tuple, *)
(* so it displays expression, expression. *)

(* App expression is the exact same as the fun expression. It displays *)
(* expression, expression. *)

(*Seq expression utilizes a helper function to display since it is a list. It *)
(* iterates through the expression list and displays each expression seperated by a comma. *)

(* Disp expression is super simple, it just displays the expression associated with it. *)

(*Closure expression is a two expression tuple, so it displays just that. *)

(*Nothing expression is nothing, so I just had it say null like other languages. *)

fun e2s (Bool b) = "Bool(" ^ (Bool.toString b) ^ ")"
  | e2s (Int i)  = "Int(" ^ (Int.toString i) ^ ")"
  | e2s (Add (x, y)) = "Add(" ^ (e2s x) ^ "," ^ (e2s y) ^ ")"
  | e2s (If (cond, conseq, alt)) = "If(" ^ (e2s cond) ^ "," ^ (e2s conseq) ^ "," ^ (e2s alt) ^ ")"
  | e2s (Ident s) = "Ident(" ^ s ^ ")"
  | e2s (Let (l, e)) = "Let([" ^ (e2s_let_helper l) ^ "]," ^ (e2s e) ^ ")"
  | e2s (Def (s, e)) = "Def(" ^ s ^ "," ^ (e2s e) ^ ")"
  | e2s (Fun (e, x)) = "Fun(" ^ (e2s e) ^ "," ^ (e2s x) ^ ")"
  | e2s (App (f, p)) = "App(" ^ (e2s f) ^ "," ^ (e2s p) ^ ")"
  | e2s (Seq s) = "Seq([" ^ (e2s_seq_helper s) ^ "])"
  | e2s (Disp d) = "Disp(" ^ (e2s d) ^ ")"
  | e2s (Closure (e, x, _)) = "Closure(" ^ (e2s e) ^ "," ^ (e2s x) ^ ")"
  | e2s (Nothing) = "(null)"
    and e2s_let_helper ([]) = ""
      | e2s_let_helper ((id:string, e:Expr)::[]) = (id ^ ":" ^ (e2s e))
      | e2s_let_helper ((id:string, e:Expr)::tail) =
            (id ^ ":" ^ (e2s e) ^ "," ^ (e2s_let_helper tail))
    and e2s_seq_helper ([]) = ""
      | e2s_seq_helper (head::[]) = (e2s head)
      | e2s_seq_helper (head::tail) =
            (e2s head) ^ "," ^ (e2s_seq_helper tail);


(**********************************************************************************************************)

(* eval function for each expression type. *)
(* This function takes an expression as input, any of the ones defined *)
(* earlier and evaluates them according to what they are intended to do while placing them in an environment. For *)
(* instance, Add will see if both expression are integers. If they are, it will *)
(* add them together, otherwise raises on exception. Use this function if you have an expression *)
(* of any kind, and you want to evaluate it to find the value in its evaluation and place it *)
(* into the environment it was created in. *)

(*Ident expression is the implest to evaluate. It just returns a new environment *)
(* paired with the previous enviornment it was in. It gets sent to env.sml to become *)
(* its own environment. *)

(* Add expression just takes two inputs, Int expression, and adds them together as *)
(* if they were integers. If one or both of the inputs isn't an Int, an exception is raised. *)

(* Cond expression evaluates the conditional expression If the condition returns true, then evaluate conseq. *)
(* If the condition returns false, evaluate the  alt. If the condition isn't a boolean, this evaluation *)
(* cannot be done, so return an exception. *)

(* Let expression utilizes a helper function to setup the environment with the bindings *)
(* given. The helper works exactly the same as the previous Let helper function, except it sets them *)
(* into an environment instead of just turning them into strings. Evaluate the expression afterwards and return the result with the original *)
(* environment given as input. *)

(* Def expression binds the definition to the environment from input and returns *)
(* the environment. Since a value has to be returned as well, but we really don't evaluate *)
(* anything from this, just return Nothing expression as the value. *)

(* Seq expression utilizes a helper function just like in the e2s function. Since Seq is *)
(* is a list, this function iterates through list, and binds each expression to the environment *)
(* and evaluates each expression. *)

(* Disp expression is responsible for turning the expression into its string *)
(* form and printing it out. Since we don't need to bind anything to an environment, *)
(* it simply just prints out the expression and return nothing as the value. *)

(* Fun expression creates and returns a closure containing the environment at the time of *)
(* function creation. Since a closure encapsulates a function, it makes sense that the *)
(* closure expression must be evaluated in this as well. *)

(* App expression finds the closure for the function binded in the environment. It then *)
(* runs the closure with the inputs given to the function. Return the value of the function with the original *)
(* environment at the end. If any components of the input are not identifiers at their lowest form, raise an exception. *)

(*Nothing expression doesn't need an evaluation, since it has no value. A catch all case is put *)
(* at the end just before the two helper functions. *)

fun eval (env:Expr Env) (Ident s) = (env, (env s))
  | eval env (Add (x, y)) =
    (case ((eval env x), (eval env y)) of ((_,Int c),(_,Int d)) => (env, Int (c + d))
        | (c, d) => raise InvalidEvaluation "Add")
  | eval env (If (cond,cons,alt)) =
    (case (eval env cond) of
          (_,Bool b) => if b then (eval env cons)
                        else (eval env alt)
        | (_) => raise InvalidCondition "Cond Requires Bool")
  | eval env (Let (b, e)) =
    let
        val env2 = (eval_let_helper env b)
        val (_,v) = (eval env2 e)
    in
        (env, v)
    end
  | eval env (Def (s, e)) = ((env_bind env s e), Nothing)
  | eval env (Seq s) = (eval_seq_helper env s)
  | eval env (Disp d) = ((print ((e2s d) ^ "\n")); (env, Nothing))
  | eval env (Fun (e, x)) = (env, Closure (e, x, env))
  | eval env (App (func, p)) = 
    (case (eval env func) of
          (_, Closure (arg, steps, envc)) =>
            (case arg of
                  (Ident s) =>
                    let
                      val (_, param) = (eval env p)
                      val (_, return) = (eval (env_bind envc s param) steps)
                    in
                      (env, return)
                    end
                | (_) => raise InvalidParameter "Parameter must be a Identifier")
        | (_) => raise InvalidApplication "Application must be applied to a Fun")
  | eval env expr = (env, expr)
    (* Let helper binds all of the id:value pairs in the let and returns the new
    * resulting environment *)
    and eval_let_helper env [] = env
      | eval_let_helper env ((id, v)::tail) =
        (eval_let_helper (env_bind env id v) tail)
    (* Seq helper recursively evaluates all of the expressions in the sequence
    * and returns the result of the final expression in the sequence *)
    and eval_seq_helper env [] = (env, Nothing) (* edge case catch *)
      | eval_seq_helper env (head::[]) = (eval env head)
      | eval_seq_helper env (expr::tail) =
        let
          val (e,_) = (eval env expr)
        in
          (eval_seq_helper e tail)
        end;


(*******************************************************************************************************)