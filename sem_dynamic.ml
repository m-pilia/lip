(** 
 * Interpreter with dynamic scope and deep binding of functional arguments.
 * The functions passed as arguments are converted into closures, keeping the 
 * environment active while binding the functional argument. 
 *)


#use "definitions.ml";;
#use "arithmetic.ml";;


(**
 * Determine the semantic of an expression.
 * @param e   Expression to be evaluated.
 * @param env Environment for the expression semantic evaluation.
 * @return A dval object representing the semantic of `e`.
 *)
let rec sem_exp e env = match e with
  | Eint n -> DInt n
  | Ebool b -> DBool b
  | Bigint l -> bigint_repr l
  | Castint n -> 
      (match sem_exp n env with
       | DInt n -> let v, s = cast_int n in DBigint (v, s)
       | _      -> failwith "TODO Castint")
  | Emptylist -> DList ([])
  | Cons (e, l) -> 
      (match sem_exp e env, sem_exp l env with
       | e, DList l -> DList (e :: l) (* TODO does it need type check? *)
       | _          -> failwith "TODO Cons")
  | Head l -> 
      (match sem_exp l env with
       | DList l -> List.hd l (* TODO handle hd exceptions *)
       | _       -> failwith "TODO Head")
  | Tail l       -> 
      (match sem_exp l env with
       | DList l -> DList (List.tl l) (* TODO handle tl exceptions  *)
       | _           -> failwith "TODO Tail")
  | Den x -> applyenv env x
  | Prod (a, b) -> apply_operation (sem_exp a env) (sem_exp b env) ( * ) bmul
  | Sum  (a, b) -> apply_operation (sem_exp a env) (sem_exp b env) ( + ) bsum
  | Diff (a, b) -> apply_operation (sem_exp a env) (sem_exp b env) ( - ) bsub
  | Mod  (a, b) -> apply_operation (sem_exp a env) (sem_exp b env) ( mod ) bmod
  | Div  (a, b) -> apply_operation (sem_exp a env) (sem_exp b env) ( / ) bdiv
  | Less (a, b) ->
      (match sem_exp a env, sem_exp b env with
       | DInt a, DInt b -> DBool (a < b)
       | DBigint (a, sa), DBigint (b, sb) -> DBool (bless (a, sa) (b, sb))
       | _ -> failwith "TODO Less")
  | Eq (a, b) -> DBool ((sem_exp a env) = (sem_exp b env))
  | Iszero e -> sem_exp (Or (Eq (e, Eint 0), Eq (e, Bigint ([0])))) env
  | Or (a, b) ->
      (match sem_exp a env, sem_exp b env with
       | DBool a, DBool b -> DBool (a || b)
       | _                -> failwith "TODO Or")
  | And (a, b) ->
      (match sem_exp a env, sem_exp b env with
       | DBool a, DBool b -> DBool (a && b)
       | _                -> failwith "TODO And")
  | Not e ->
      (match sem_exp e env with
       | DBool a -> DBool (not a)
       | _       -> failwith "TODO Not")
  | Pair (a, b) -> DPair (sem_exp a env, sem_exp b env)
  | Fst e ->
      (match sem_exp e env with
       | DPair (a, b) -> a
       | _            -> failwith "TODO Fst")
  | Snd e ->
      (match sem_exp e env with
       | DPair (a, b) -> b
       | _            -> failwith "TODO Snd")
  | Ifthenelse (i, t, f) ->
      (* TODO need to check types of t and f for equality? *)
      (match sem_exp i env with
       | DBool i -> if i then sem_exp t env else sem_exp f env
       | _ -> failwith "TODO Ifthenelse non-bool condition")
  | Let (l, e) -> 
      (* return a closure for functional results *)
      let env = bind_ids env l in 
      (match sem_exp e env with
       | DFun (ids, e) -> DClos (ids, e, env)
       | _ as d        -> d)
  (* TODO parameters checking; (type inference?) *)
  | Fun (l, e) -> DFun (l, e) 
  | Apply (e, args) -> (* TODO type check? *)
      (* evaluate closures in their own environment, and other functions
       * in the active environment *)
      (match sem_exp e env with
       | DClos (ids, e, cl) -> sem_exp e (bind_args env cl  ids args)
       | DFun  (ids, e)     -> sem_exp e (bind_args env env ids args)
       | _ -> failwith "TODO Apply")
  | Try _
  | Raise _ -> failwith "TODO unimplemented exceptions"

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
  | (x, e) :: t -> bind_ids (bind env x (sem_exp e env)) t

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
  (* TODO handle exceptions from sem_exp *)
  let vals = 
    List.map 
      (fun x -> match sem_exp x env with
       | DFun(l, e) -> DClos(l, e, env) (* closure for functional args *)
       | _ as d     -> d)
      args in
  try
    List.fold_left2 bind cl ids vals
  with
  | Invalid_argument m -> failwith "Mismatching function arguments"
  | _                  -> failwith "TODO"
;;

