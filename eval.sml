use "env.sml";

Control.Print.printDepth := 32;

datatype Expr
  = Bool    of bool
  | Int     of int
  | Add     of Expr * Expr
  | If      of Expr * Expr * Expr
  | Sym     of string
  | Def     of string * Expr
  | Fun     of Expr * Expr
  | Closure of Expr * Expr * Expr Env
  | App     of Expr * Expr
  | Seq     of Expr list
  | Disp    of Expr
  | Let     of (string * Expr) list * Expr
  | Nothing
  ;

fun e2s (Bool b)                   = Bool.toString b
  | e2s (Int n)                    = Int.toString n
  | e2s (If (cond, conseq, alt))   = "if " ^ (e2s cond)
                                   ^ " then " ^ (e2s conseq)
                                   ^ " else " ^ (e2s alt)
  | e2s (Sym s)                    = s
  | e2s (Def (name, expr))         = "Def(" ^ name ^ "," ^ (e2s expr) ^ ")"
  | e2s (Fun (param, expr))        = "Fun(" ^ (e2s param) ^ "," ^ (e2s expr) ^ ")"
  | e2s (Closure (param, expr, _)) = e2s (Fun (param, expr))
  | e2s (App (abstr, arg))         = (e2s abstr) ^ "(" ^ (e2s arg) ^ ")"
  | e2s (Seq exprs)                = "(" ^ e2s_seq_helper exprs
  | e2s (Disp expr)                = "Disp(" ^ (e2s expr) ^ ")"
  | e2s (Let (bindings, body))     = "Let(" ^ (e2s_bindings bindings) 
                                   ^ " in " ^ (e2s body) ^ ")"
  | e2s Nothing                    = "Nothing"

and e2s_seq_helper [] = ")"
  | e2s_seq_helper (expr :: []) = (e2s expr) ^ ")"
  | e2s_seq_helper (expr :: exprs) = (e2s expr) ^ "; " ^ (e2s_seq_helper exprs)

and e2s_bindings [] = ""
  | e2s_bindings [(lhs, rhs)] = lhs ^ "=" ^ (e2s rhs)
  | e2s_bindings (binding::rest) = (e2s_bindings [binding]) ^ ", " ^ (e2s_bindings rest)
  ;

fun go() =
  let val expr = If (Bool true, Int 100, Int 200)
  in e2s expr
  end;

fun indent 0 = ()
  | indent n = (print "| "; indent (n-1))
  ;

fun debug depth message = (indent depth; print message; print "\n");


(********************************************************************
 * eval functions for separate constructors
 ********************************************************************)
exception NotAnAbstraction of Expr;
exception NotABoolean      of Expr;
exception AddNotIntegers   of Expr* Expr;

fun eval_sym env name = env name;

fun eval_fun env param body = Closure (param, body, env);

fun eval_app env abstr arg =
  let val (_, abstr') = eval env abstr
      val (_, arg') = eval env arg
  in case abstr'
       of Closure (Sym param, body, lexEnv) =>
            let val lexEnv' = env_bind lexEnv param arg'
                val (_, result) = eval lexEnv' body
            in (env, result) (* must return the original environment *)
            end
        | _ => raise NotAnAbstraction abstr'
  end

and eval_def env name expr =
  let val (env', value) = eval env expr
      val env'' = env_bind env' name value
  in (env'', value)
  end

and eval_if env cond conseq alt =
  let val (env', cond') = eval env cond
  in case cond'
       of Bool true => eval env' conseq
        | Bool false => eval env' alt
        | _ => raise NotABoolean cond'
  end

and eval_let origEnv localEnv [] body =
      let val (_, res) = eval localEnv body
      in (origEnv, res) (* must return the original environment *)
      end
  | eval_let origEnv localEnv ((name, rhs)::bindings) body =
      let val (_, res) = eval localEnv rhs
          val localEnv' = env_bind localEnv name res
      in eval_let origEnv localEnv' bindings body
      end

and eval_add env lhs rhs =
      let val (env', lhs') = eval env lhs
          val (env'', rhs') = eval env' rhs
      in case (lhs', rhs')
           of (Int x, Int y) => (env, Int (x + y))
            | _ => raise AddNotIntegers (lhs', rhs')
      end

(********************************************************************
 * main umbrella eval function
 ********************************************************************)
and eval (env: Expr Env) (i as Int _) : (Expr Env * Expr) = (env, i)
  | eval env (b as Bool _) = (env, b)
  | eval env (If (cond, conseq, alt)) = eval_if env cond conseq alt
  | eval env (Fun (param, body)) = (env, Closure (param, body, env))
  | eval env (Let (bindings, body)) = eval_let env env bindings body
  | eval env (Add (lhs, rhs)) = eval_add env lhs rhs
  | eval env (Sym name) = (env, env name)
  | eval env (App (func, arg)) = eval_app env func arg
  | eval env expr = (env, expr) (* catch-all *)
  ;

val env0  = env_new () : Expr Env;
val env1  = env_bind env0 "a" (Int 100);
val env2  = env_bind env1 "b" (Int 200);
val env3  = env_bind env2 "c" (Int 300);

val expr1 = Fun(Sym "x", Sym "x");
val expr2 = App(expr1, Int 100);
val expr3 = App(expr1, Nothing);

(*fun go() = eval env0 expr2;*)

fun l() = use "eval.sml";

val lt = Let ([("x",Int 100),("y",Int 200)],Add (Sym "x",Sym "y"));

val add1 = Fun (Sym "x", Add (Sym "x", Int 1));

val ap = App (add1, Int 2);
