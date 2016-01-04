(**
 * Convert an abstract syntax expressions into a string containing its
 * OCaml generator code.
 *)


(**
 * Convert a list of generic objects into a string.
 * @param pp A function that convert the generic object to string.
 * @param l  The list to be converted.
 * @return A string representation of the list.
 *)
let string_of_list pp l = 
  let rec solr = function
    | []      -> "]"
    | h :: [] -> pp h ^ "]"
    | h :: t  -> pp h ^ "; " ^ solr t in
  "[" ^ solr l
;;


(**
 * Convert a list of int values into a string.
 * @param l  The list to be converted.
 * @return A string representation of the list.
 *)
let string_of_int_list l = string_of_list string_of_int l
;;


(**
 * Convert a list of string values into a string.
 * @param l  The list to be converted.
 * @return A string representation of the list.
 *)
let string_of_string_list l = string_of_list (fun x -> "\"" ^ x ^ "\"") l
;;


(**
 * Convert an expression into a string.
 * @param e The expression to be converted
 * @return A string representation of the expression.
 *)
let rec string_of_exp = function
  | Eint n -> "Eint (" ^ string_of_int n ^ ")"
  | Ebool b -> "Ebool (" ^ string_of_bool b ^ ")"
  | Bigint l -> "Bigint (" ^ string_of_int_list l ^ ")"
  | Castint e -> "Castint (" ^ string_of_exp e ^ ")"
  | Emptylist -> "Emptylist"
  | Cons (e1, e2) -> 
      "Cons (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | Head e -> "Head (" ^ string_of_exp e ^ ")"
  | Tail e -> "Tail (" ^ string_of_exp e ^ ")"
  | Den x -> "Den (\"" ^ x ^ "\")"
  | Prod (e1, e2) -> 
      "Prod (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | Sum (e1, e2) -> 
      "Sum (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | Diff (e1, e2) -> 
      "Diff (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | Mod (e1, e2) -> 
      "Mod (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | Div (e1, e2) -> 
      "Div (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | Less (e1, e2) -> 
      "Less (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | Eq (e1, e2) -> 
      "Eq (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | Iszero e -> "Iszero (" ^ string_of_exp e ^ ")"
  | Or (e1, e2) -> 
      "Or (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | And (e1, e2) -> 
      "And (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | Not e -> "Not (" ^ (string_of_exp e) ^ ")"
  | Pair (e1, e2) -> 
      "Pair (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | Fst e -> "Fst (" ^ string_of_exp e ^ ")"
  | Snd e -> "Snd (" ^ string_of_exp e ^ ")"
  | Ifthenelse (b, t, f) -> 
      "Ifthenelse (" ^ string_of_exp b ^ ", " ^ string_of_exp t ^ ", " 
       ^ string_of_exp f ^ ")"
  | Let (l, e) -> 
      "Let (" ^ string_of_bind_list l ^ ", " ^ string_of_exp e ^ ")"
  | Fun (l, e) ->
      "Fun (" ^ string_of_string_list l ^ ", " ^ string_of_exp e ^ ")"
  | Apply (e, l) ->
      "Apply (" ^ string_of_exp e ^ ", " ^ string_of_exp_list l ^ ")"

(**
 * Convert a list of binds, expressed as couples (identifier, expression), 
 * into a string.
 * @param l  The list to be converted.
 * @return A string representation of the list.
 *)
and string_of_bind_list l = 
  string_of_list (fun (s, e) -> "(\"" ^ s ^ "\", " ^ string_of_exp e ^ ")") l

(**
 * Convert a list of expressions into a string.
 * @param l  The list to be converted.
 * @return A string representation of the list.
 *)
and string_of_exp_list l =
  string_of_list (fun x -> string_of_exp x) l
;;

