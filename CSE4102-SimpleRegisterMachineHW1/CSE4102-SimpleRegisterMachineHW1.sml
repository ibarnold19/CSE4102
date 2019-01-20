(* Simple Register Machine (Homework 1) *)
(* CSE 4102 Project 01, Spring Semester, 2018 *)
(* Bryan Arnold *)
(* 2/3/2018 *)
(*Section: 001 *)
(* Instructor: Jeffrey A. Meunier *)

(* Type definition for MachineStates. A MachineState type *)
(* keeps track of two values that are associated with it, *)
(* interger a and integer b *);
type MachineState = {a:int, b:int};

(* Constructor for the MachineState type. Use *)
(* this to create a MachineType type *)
fun newMachine (a, b) : MachineState = {a=a, b=b};

(* getA and getB functions *)
(* These functions get the values of a and b *)
(* from the provided MachineState. Use this if *)
(* you want to get the values of a or b in a given *)
(* MachineState. *)
fun getA (m : MachineState) = #a m;
fun getB (m : MachineState) = #b m;

(* setA and setB functions *)
(* These functions set the values of a and b *)
(* from the provided MachineState by creating *)
(* a new state with the desired new value, and the other *)
(* old value. Use these functions to change the values *)
(* either in a or b in a given MachineState. *)
fun setA (m : MachineState, newA) = newMachine (newA, #b m);
fun setB (m : MachineState, newB) = newMachine (#a m, newB);

(* Datatype defintion for the Instruction data type. *)
(* This type is used to define what instructions are permissible *)
(* and what can be used by the program. *)
datatype Instruction = SetA of int | SetB of int | Add | Sub | Disp;

(*i2s function takes an instruction *)
(* and turns it into it's string form. *)
(* Use this when you need to turn an instruction *)
(* into a string to be printed *)
fun i2s (SetA a) = "SetA " ^ Int.toString(a)
 | i2s (SetB b) = "SetB " ^ Int.toString(b)
 | i2s Add = "Add"
 | i2s Sub = "Sub"
 | i2s Disp = "Disp"
 ;

(* Default instruction list for the given problem *)
[SetA 10, SetB 2, Add, SetB 4, Sub, Disp];

(* eval function takes a state and an instruction *)
(* in the given format, and computes what was *)
(* desired by the function. Use this to evaluate *)
(* an instruction by replacing a MachineStates' value *)
(* for a or b, add b into a, subtract b from a, or display *)
(* the value of a *);
fun eval (m : MachineState, SetA a) : MachineState =
      setA(m, a) 
  | eval (m : MachineState, SetB b) : MachineState = 
  	  setB(m, b)
  | eval (m : MachineState, Add ) : MachineState = 
      setA(m, (#a m) + (#b m))
  | eval (m : MachineState, Sub ) : MachineState = 
      setA(m, (#a m) - (#b m))
  | eval (m : MachineState, Disp) : MachineState =
 	 (print (Int.toString (#a m) ^ "\n");
 	  m)
 ;

(*This function iterates through an instruction list *)
(* recurrsively, prints the current instruction being *)
(* looked at, call the eval function on it, then repeats *)
(* until the end of the list is reached. Use this to *)
(* evaluate multiple instructions in an instruction list. *)
fun run (m : MachineState, [] : Instruction list) = m
  | run (m : MachineState, prog : Instruction list) =
      let val instr = (hd prog)			(* next instruc in prog list *)
 	 	  val instrs = (tl prog) 		(* rest of prog list *)
 	 	  val _ = print (i2s instr ^ "\n"); 
 	 	  val m1 = eval (m, instr);
 	  in  		
 		 run (m1, (tl prog)) 			(* recursive call *)
 	  end;