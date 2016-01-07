(** 
 * Shared type definitions.
 *)
 

(* ************************************ *
 *                                      *
 *           TYPE DEFINITIONS           *
 *                                      *
 * ************************************ *)


(** Type for identifiers. *)
type ide = string
;; 


(** Type for a sign. *)
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

(** Semantic domain for the environment. *)
and dval =
  | Unbound
  | DInt    of int
  | DBool   of bool
  | DBigint of int list * sign
  | DList   of dval list
  | DPair   of dval * dval
  | DClos   of ide list * exp * env
  | DFun    of ide list * exp

(** Type for the environment. *)
and env = Env of (ide -> dval)
;;


(**
 * Generate a list in abstract syntax from an OCaml list.
 * @param l List of exp elements.
 * @return A Cons object containing the elements from `l`.
 *)
let rec cons_of_list = function
  | []     -> Emptylist
  | h :: t -> Cons(h, cons_of_list t)
;;


(**
 *
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
  | _ -> failwith "DUMP FAIL"
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

