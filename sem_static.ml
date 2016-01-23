(** 
 * Interpreter with static scope rule.
 * A function is represented as a closure, keeping the environment
 * at the time of the function definition.
 *
 * Detailed documentation for each feature is provided along type and function 
 * definitions in the `definitions.ml` and `arithmetic.ml` files.
 *)


#use "definitions.ml";;
#use "arithmetic.ml";;


(**
 * Determine the semantic of an expression.
 * @param e   Expression to be evaluated.
 * @param env Environment for the expression semantic evaluation.
 * @return A dval object representing the semantic of `e`.
 *)
let rec sem_static e env = match e with
  | Eint n -> DInt n
  | Ebool b -> DBool b
  | Bigint l -> let d, s = bigint_repr l in DBigint (d, s)
  | Castint n -> 
      (match sem_static n env with
       | DInt n -> let v, s = cast_int n in DBigint (v, s)
       | _      -> failwith "Castint: invalid operand type")
  | Emptylist -> DList ([])
  | Cons (e, l) -> 
      (match sem_static e env, sem_static l env with
       | e, (DList l as ll) -> 
           if (TList (type_repr e)) === (type_repr ll) then
             DList (e :: l)
           else
             failwith "Cons: mismatching list type"
       | _          -> failwith "Cons: invalid operand type, not a list")
  | Head l -> 
      (match sem_static l env with
       | DList (h :: t) -> h
       | DList ([])     -> failwith "Head: empty list"
       | _              -> failwith "Head: invalid operand type, not a list")
  | Tail l -> 
      (match sem_static l env with
       | DList (h :: t) -> DList (t)
       | DList ([])     -> failwith "Tail: empty list"
       | _              -> failwith "Tail: invalid operand type, not a list")
  | Den x -> applyenv env x
  | Prod (a, b) -> 
      apply_operation (sem_static a env) (sem_static b env) ( * ) bmul
  | Sum  (a, b) ->
      apply_operation (sem_static a env) (sem_static b env) ( + ) bsum
  | Diff (a, b) ->
      apply_operation (sem_static a env) (sem_static b env) ( - ) bsub
  | Mod  (a, b) ->
      apply_operation (sem_static a env) (sem_static b env) ( mod ) bmod
  | Div  (a, b) ->
      apply_operation (sem_static a env) (sem_static b env) ( / ) bdiv
  | Less (a, b) ->
      (match sem_static a env, sem_static b env with
       | DInt a, DInt b -> DBool (a < b)
       | DBigint (a, sa), DBigint (b, sb) -> DBool (bless (a, sa) (b, sb))
       | _ -> failwith "Less: invalid operand type")
  | Eq (a, b) -> DBool ((sem_static a env) = (sem_static b env))
  | Iszero e -> sem_static (Or (Eq (e, Eint 0), Eq (e, Bigint ([0])))) env
  | Or (a, b) ->
      (match sem_static a env, sem_static b env with
       | DBool a, DBool b -> DBool (a || b)
       | _                -> failwith "Or: invalid operand type")
  | And (a, b) ->
      (match sem_static a env, sem_static b env with
       | DBool a, DBool b -> DBool (a && b)
       | _                -> failwith "And: invalid operand type")
  | Not e ->
      (match sem_static e env with
       | DBool a -> DBool (not a)
       | _       -> failwith "Not: invalid operand type")
  | Pair (a, b) -> DPair (sem_static a env, sem_static b env)
  | Fst e ->
      (match sem_static e env with
       | DPair (a, b) -> a
       | _            -> failwith "Fst: invalid operand type")
  | Snd e ->
      (match sem_static e env with
       | DPair (a, b) -> b
       | _            -> failwith "Snd: invalid operand type")
  | Ifthenelse (i, t, f) ->
      (* it is safe to evaluate both the branches because no recursion is
       * allowed in the statically scoped language *)
      let tsem, fsem = sem_static t env, sem_static f env in
      (* type check of the branch expressions *)
      if (type_repr tsem) === (type_repr fsem) then
        (match sem_static i env with
         | DBool i -> if i then tsem else fsem
         | _ -> failwith "Ifthenelse: non-bool condition")
      else
        failwith "Ifthenelse: type mismatch"
  | Let (l, e) -> let env = bind_ids env l in sem_static e env
  | Fun (l, e) -> DClos (l, e, env) 
  | Apply (e, args) ->
      (match sem_static e env with
       | DClos (ids, e, cl) -> sem_static e (bind_args env cl ids args)
       | _ -> failwith "Apply: cannot apply non-functional argument")
  | Try _
  | Raise _ -> failwith ("Exceptions are not implemented here, " ^ 
                         "see sem_static_exceptions.ml")

(** 
 * Bind a list of identifiers to their values into an environment. 
 * @param env Environment where the identifiers will be binded.
 * @param l   List of couples `(i, e)` of identifiers and expressions.
 * @return An environment containing all the bindings of `env` and where
 *         each identifier `i` in `l` is binded with the evaluation result
 *         of its coupled expression `e`.
 *)
and bind_ids env l = match l with
  | [] -> env
  | (x, e) :: t -> bind_ids (bind env x (sem_static e env)) t

(** 
 * Evaluate a list of arguments and bind them to their identifiers into 
 * an environment. 
 * @param env  Environment for the evaluation of the arguments.
 * @param cl   Closure environment for the function.
 * @param ids  Identifiers of the arguments.
 * @param args Expressions passed to the arguments.
 * @return An environment containing all the bindings of `cl` plus the 
 *         bindings for the arguments.
 *)
and bind_args env cl ids args =
  let vals = List.map (fun x -> sem_static x env) args in
  try
    List.fold_left2 bind cl ids vals
  with
  | Invalid_argument _ 
  | _                  -> failwith "Mismatching function arguments"
;;

