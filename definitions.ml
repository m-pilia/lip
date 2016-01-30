(** 
 * Definitions of shared types and functions.
 *)
 

(* ************************************ *
 *                                      *
 *           TYPE DEFINITIONS           *
 *                                      *
 * ************************************ *)


(** Type for identifiers. *)
type ide = string
;; 


(** Type for the sign. *)
type sign =
  | Positive
  | Negative
;;
 

(** Type for expressions. *)
type exp =  
  | Eint       of int       (** Integer. *)
  | Ebool      of bool      (** Boolean. *)
  | Bigint     of int list  (** Big integer (unrestricted number of digits). *)
  | Castint    of exp       (** Cast from int to bigint. *)
  | Emptylist               (** Empty list. *)
  | Cons       of exp * exp (** Add an element to a list. *)
  | Head       of exp       (** Head element of a list. *)
  | Tail       of exp       (** Tail element of a list. *)
  | Den        of ide       (** Variable. *)
  | Prod       of exp * exp (** Polymorphic product of two expressions. *)
  | Sum        of exp * exp (** Polymorphic sum of two expressions. *)
  | Diff       of exp * exp (** Polymorphic difference of two expressions. *)
  | Mod        of exp * exp (** Polymorphic modulus of two expressions. *)
  | Div        of exp * exp (** Polymorphic division of two expressions. *)
  | Less       of exp * exp (** Polymorphic unequality of two expressions. *)
  | Eq         of exp * exp (** Polymorphic equality between two expressions. *)
  | Iszero     of exp       (** State wether an expression has value zero. *)
  | Or         of exp * exp (** Logical or. *)
  | And        of exp * exp (** Logical and. *)
  | Not        of exp       (** Logical negation. *)
  | Pair       of exp * exp (** Pair of expressions. *)
  | Fst        of exp       (** Return the first expression of a pair. *)
  | Snd        of exp       (** Return the second expression of a pair. *)
  | Ifthenelse of exp * exp * exp        (** Condition-based choiche. *)
  | Let        of (ide * exp) list * exp (** Block declaration. *)
  | Fun        of ide list * exp         (** Functional abstraction. *)
  | Apply      of exp * exp list         (** Function evaluation. *)
  | Try        of exp * ide * exp        (** Exception guard. *)
  | Raise      of ide                    (** Raise an exception. *)

(** 
 * Semantic domain for the environment.
 *
 * ## Bigint
 *
 * The big integer is represented as a couple of a list of digit chunks and a 
 * sign. Each chunk contains `log_base` digits. The head of the list 
 * contains the chunk with the least significative digits, and each following 
 * chunk contains digits more significative than the preceeding. Each chunk of
 * digits is represented as an integer value.
 *
 * If the list elements for the digits are `d0` ... `dn`, the represented 
 * value is equal to: 
 *   `d0` * 10^(`log_base` * 0) + ... + `dn` * 10^(`log_base` * n)
 *
 *
 * ## Functions and closures
 *
 * A function is represented as a list of identifiers for its parameters and
 * an expression. A closure adds an environment for the evaluation of the 
 * expression.
 *)
and dval =
  | Unbound
  | DInt    of int                  (** Integer value. *)
  | DBool   of bool                 (** Boolean value. *)
  | DBigint of int list * sign      (** Big integer value. *)
  | DList   of dval list            (** List value. *)
  | DPair   of dval * dval          (** Pair value. *)
  | DClos   of ide list * exp * env (** Closure value. *)
  | DFun    of ide list * exp       (** Function value. *)
  | DExc    of ide                  (** Exception value. *)

(** Type for the environment. *)
and env = Env of (ide -> dval)
;;


(* ************************************ *
 *                                      *
 *              ENVIRONMENT             *
 *                                      *
 * ************************************ *)


(** 
 * Exception risen when trying to retry the value of an unbounded identifier
 * from an environment.
 *)
exception UnboundException of ide
;;


(**
 * Exception risen when the evaluation of an argument during a bind operation
 * returns an exception.
 *)
exception BindException of dval
;;


(** 
 * Return a new, empty environment.
 * @return An Env object wherein all identifiers are unbound.
 *)
let emptyenv () = Env (fun x -> Unbound)
;;


(** 
 * Extend the environment with a new bind. 
 * @param e Enviroment to be extended.
 * @param x Identifier for the new bound.
 * @param v Value to be bounded to the identifier.
 * @return An Env object containing all the bounds of e plus the new bound.
 *)
let bind (Env env) x v = Env (fun o -> if o = x then v else env o)
;;


(** 
 * Return the value bounded to an identifier.
 * @param e Environment containing the bounded object.
 * @param x Identifier for the object.
 * @return A val object containing the value bounded to the identifier.
 * @raise UnboundException When the identifier x is unbounded inside the
 *        environment e.
 *)
let applyenv (Env e) x = match e x with
  | Unbound -> raise (UnboundException x)
  | v       -> v
;;


(* ************************************ *
 *                                      *
 *                TYPES                 *
 *                                      *
 * ************************************ *)


(** 
 * Type to denote types of objects in the semantic domain of the 
 * environment.
 *)
type _type = 
    TInteger 
  | TBiginteger 
  | TBoolean 
  | TException 
  | TEmptylist
  | TList of _type 
  | TPair of _type * _type
  | TFunction
;;


(**
 * Return the type of an environment object.
 * @param v Object of type `dval`.
 * @return Type of `v`, expressed with a `_type` object.
 *)
let rec type_repr v = match v with
  | DInt _         -> TInteger
  | DBool _        -> TBoolean
  | DBigint _      -> TBiginteger
  | DList ([])     -> TEmptylist
  | DList (h :: t) -> TList (type_repr h)
  | DPair (l, r)   -> TPair (type_repr l, type_repr r)
  | DClos _        
  | DFun _         -> TFunction
  | DExc _         -> TException 
  | Unbound        -> failwith "Invalid object"
;;


(**
 * Return the type of an expression. 
 * This is a partial function because it is not always possible to determine 
 * the type without a more complete mechanism of inference. 
 * In detail, this function cannot rely on the evalutation of the expression 
 * or any expression contained in it, because this may create an indefinite 
 * loop when dealing with recursive functions. For this reason, complex 
 * expressions like function objects, Let objects or function calls cannot
 * be analyzed.
 * @param e   Object of type `exp`.
 * @param env Environment for the expression.
 * @return An `option` object equal to `None` if the type cannot be desumed,
 *         or equal to `Some t` where `t` is the desumed `_type` object
 *         indicating the type of `e`.
 *)
let rec type_exp e env = match e with
  | Eint _ -> Some TInteger
  | Ebool _ -> Some TBoolean
  | Bigint _
  | Castint _ -> Some TBiginteger 
  | Emptylist -> Some TEmptylist
  | Cons (e, _) -> 
      (match type_exp e env with
       | None -> None
       | Some t -> Some (TList (t)))
  | Head l -> 
      (match type_exp l env with
       | (Some TList t) -> Some t
       | _              -> None)
  | Tail l -> type_exp l env
  | Den x -> Some (type_repr (applyenv env x))
  | Prod (a, b)
  | Sum  (a, b)
  | Diff (a, b)
  | Mod  (a, b)
  | Div  (a, b) -> 
      (match type_exp a env, type_exp b env with
       | Some TBiginteger, _
       | _, Some TBiginteger          -> Some TBiginteger
       | Some TInteger, Some TInteger -> Some TInteger
       | _                            -> None)
  | Less _
  | Eq _
  | Iszero _ 
  | Or _ 
  | And _
  | Not _ -> Some (TBoolean)
  | Pair (a, b) -> 
      (match type_exp a env, type_exp b env with
       | None, _
       | _, None -> None
       | Some t, Some s -> Some (TPair (t, s)))
  | Fst (Pair(a, b)) -> type_exp a env
  | Fst (Den x) ->
      (match type_repr (applyenv env x) with
       | TPair (a, b) -> Some a
       | _                   -> None)
  | Snd (Pair(a, b)) -> type_exp b env
  | Snd (Den x) ->
      (match type_repr (applyenv env x) with
       | TPair (a, b) -> Some b
       | _                   -> None)
  | Ifthenelse _
  | Let _
  | Fun _
  | Apply _
  | Try _
  | Raise _
  | _ -> None
;;


(**
 * Check two types for equality.
 * Two types are equal if they are the same type, if one is an exception, or if 
 * one is a list of any type and the other is an empty list.
 * @param l Left operand.
 * @param r Right operand.
 * @return True if the two operands have the same type, false otherwise.
 *)
let ( === ) l r =
  l = r ||
  match l, r with
  | TException, _
  | _, TException
  | TList _, TEmptylist
  | TEmptylist, TList _ -> true
  | _                   -> false
;;


(**
 * Check two optional types for equality.
 * With a conservative approach due to the non-completeness of the type
 * check for expressions, two types are assumed to be equal when they are 
 * actually equal or when one of them cannot be identified.
 * @param l Left operand.
 * @param r Right operand.
 * @return True if the two operands have the same type, false otherwise.
 *)
let ( =~= ) l r =
  match l, r with
  | None, _
  | _, None -> true
  | Some t, Some s when t === s -> true
  | _ -> false
;;


(**
 * Generate a list in abstract syntax from an OCaml list.
 * @param l List of `exp` elements.
 * @return A `Cons` object containing the elements from `l`.
 *)
let rec cons_of_list = function
  | []     -> Emptylist
  | h :: t -> Cons(h, cons_of_list t)
;;


(**
 * Convert a `dval` object into a string.
 * @param v A `dval` object.
 * @return The string representation of `v`.
 *)
let rec dump_dval v = match v with
  | DInt n -> Printf.sprintf "DInt (%d)" n
  | DBool b -> Printf.sprintf "DBool (%B)" b
  | DBigint (l, s) -> 
      "DBigint (" ^
      "[" ^ (String.concat "; " (List.map string_of_int l)) ^ "], " ^
      (if s = Positive then "Positive" else "Negative") ^ ")"
  | DList l -> 
      "DList (" ^
      "[" ^ (String.concat "; " (List.map dump_dval l)) ^ "]" ^
      ")"
  | DPair (l, r) -> 
      "DPair (" ^
      (dump_dval l) ^ ", " ^
      (dump_dval r) ^ ")"
  | DClos (l, e, cl) ->
      "DClos (" ^ 
      "[" ^ (String.concat "; " l) ^ "], " ^
      "some stuff" ^ ")"
  | DFun (l, e) ->
      "DFun (" ^ 
      "[" ^ (String.concat "; " l) ^ "], " ^
      "some stuff" ^ ")"
  | DExc i -> "DExc \"" ^ i ^ "\""
  | _ -> failwith "DUMP FAIL"
;;

