(** 
 * Ausiliary arithmetic functions.
 *)

(** 
 * Plus and minus sign, encoded as their ASCII value minus the ASCII value of
 * '0'.
 *)
let plus, minus = -5, -3
;;


(** 
 * Invert a sign.
 * @param s Sign to be inverted.
 * @return Positive when `s` is negative, Negative otherwise.
 *)
let inv_sign s = match s with
  | Positive -> Negative
  | Negative -> Positive
;;


(**
 * Convert a string into a Bigint value.
 * @param s String formed by an optional sign ('+', '-') followed by the 
 *          succession of digits of the value.
 * @return A Bigint value corresponding to the input string.
 *)
let str_to_bigint s =
  let rec stbr i l =
    if i < 0 then 
      l  
    else
      let v = (Char.code(s.[i]) - (Char.code '0')) in
      if (v >= 0 && v <= 9) || v = plus || v = minus then
        stbr (i - 1) (v :: l)
      else 
        failwith "The bigint representation contains invalid characters" in
  Bigint (List.rev (stbr (String.length s - 1) []))
;;


(**
 * Convert a list of digits from a Bigint object into a corresponding DBigint 
 * object.
 * @param l The list of sign and digits of the Bigint object.
 * @return The corresponding DBigint object.
 *)
let bigint_repr l = 
  match List.rev l with
  | [] -> DBigint ([], Positive)
  | h :: t when h = minus -> DBigint (List.rev t, Negative)
  | h :: t when h = plus  -> DBigint (List.rev t, Positive)
  | h :: t when h >= 0 && h <= 9 -> DBigint (List.rev (h :: t), Positive)
  | _ -> failwith "TODO bigint representation conversion";
;;


(** 
 * Return the list of the digits forming the input integer value. The list
 * goes from the least to most significative digit.
 * @param n Integer value.
 * @return A list of the digits forming n.
 *)
let cast_int n =
  let sign = if n >= 0 then Positive else Negative in
  let n = abs n in
  let rec cintr n l =
    if n < 10 then List.rev (n :: l) else cintr (n / 10) ((n mod 10) :: l) in
  cintr n [], sign
;;


(**
 * Sort two Bigint values respect to their absolute value.
 * @param a  First value (list of int digits).
 * @param sa First value's sign.
 * @param b  Second value (list of int digits).
 * @param sb Second value's sign.
 *)
let bsortabs (a, sa) (b, sb) =
  let la, lb = List.length a, List.length b in
  if la > lb then
    (a, sa), (b, sb)
  else if lb > la then
    (b, sb), (a, sa)
  else
    let ta, tb = List.hd (List.rev a), List.hd (List.rev b) in
    if ta >= tb then
      (a, sa), (b, sb)
    else
      (b, sb), (a, sa)
;;


(**
 * Three ways comparator for two positive lists of digits.
 * @param a Digits of the first value.
 * @param b Digits of the second value.
 * @return -1 if `a` > `b`, 0 if `a` = `b`, 1 if `a` < `b`.
 *)
let rec cmp_list a b =
  let la, lb = List.length a, List.length b in
  if a = b then
    0
  else if la > lb then
    -1
  else if lb > la then
    1
  else
    let ra, rb = List.rev a, List.rev b in
    let ta, tb = List.hd ra, List.hd rb in
    if ta > tb then
      -1
    else if tb > ta then
      1
    else
      cmp_list (List.rev (List.tl ra)) (List.rev (List.tl rb))
;;


(**
 * Lesser operator for Bigint values.
 * @param a  First term (list of int digits).
 * @param sa First term's sign.
 * @param b  Second term (list of int digits).
 * @param sb Second term's sign.
 * @return True if a is lesser than b, false otherwise.
 *)
let bless (a, sa) (b, sb) = match sa, sb with
  | Positive, Negative -> false
  | Negative, Positive -> true
  | Positive, Positive -> (cmp_list a b) = 1
  | Negative, Negative -> (cmp_list a b) = -1 
;;


(**
 * Trim leading zeros from a list of digits.
 * @param x List of digits to be trimmed.
 * @return The `x` list without any sequence of zeros at the end.
 *)
let rec zero_trim x =
  let rec ztr = function
      []     -> [0]
    | 0 :: t -> ztr t
    | _ as t -> t in
  List.rev (ztr (List.rev x))
;;


(**
 * Sum two positive integers expressed as lists of digits.
 * @param a First addend.
 * @param b Second addend.
 * @return The a + b sum.
 *)
let rec bsump a b =
  (** 
   * @param a   Digits of the first addend to be consumed.
   * @param b   Digits of the second addend to be consumed.
   * @param sum Sum of the consumed digits.
   * @param c   Carry of the last digit addition.
   * @return The a + b sum when all the input digits have been consumed.
   *)
  let rec bsumr a b sum c = match a, b with
    | [], []
    | [0], [0] -> List.rev (if c = 0 then sum else c :: sum) 
    | x, []
    | x, [0]
    | [], x
    | [0], x -> 
        let x = if x = [] then [0] else x in
        if c = 0 then
          (List.rev sum) @ x
        else
          (List.rev sum) @ (bsumr x [c] [] 0)
    | ha :: ta, hb :: tb ->
        let s = ha + hb + c in
        bsumr ta tb ((s mod 10) :: sum) (s / 10) in
  zero_trim (bsumr a b [] 0)
;;


(** 
 * Subtract two positive integers expressed as lists of digits.
 * @param a Minuend.
 * @param b Subtraend.
 * @return The a - b subtraction.
 *)
let bsubp a b =
  (**
   * @param a   Digits of the first term to be consumed.
   * @param b   Digits of the second term to be consumed.
   * @param sum Subtraction of the consumed digits.
   * @param c   Carry of the last digit subtraction.
   * @return The a - b subtraction when all the input digits have been consumed.
   *)
  let rec bsubr a b sub c = match a, b with
    | [], []
    | [0], [0] -> List.rev sub 
    | x, []
    | x, [0] -> 
        if c = 0 then 
          (List.rev sub) @ x
        else
          (List.rev sub) @ (bsubr x [c] [] 0)
    | ha :: ta, hb :: tb ->
        let s = ha - hb - c in
        let c = if s < 0 then 1 else 0 in
        let s = if s < 0 then 10 + s else s in
        bsubr ta tb (s :: sub) c
    | _ -> failwith ("Second term smaller than first " ^ 
                    (string_of_int (List.length a)) ^ 
                    " " ^
                    (string_of_int (List.length b))) in
  zero_trim (bsubr a b [] 0)
;;


(** 
 * Sum two Bigint values, represented as lists of digits.
 * @param a  First addend (list of int digits).
 * @param sa First addend's sign.
 * @param b  Second addend (list of int digits).
 * @param sb Second addend's sign.
 * @return The a + b sum represented as a list of digits.
 *)
let bsum (a, sa) (b, sb) = match sa, sb with
  | Positive, Positive
  | Negative, Negative -> bsump a b, sa 
  | Positive, Negative  
  | Negative, Positive -> 
      let (big, sbig), (small, ssmall) = bsortabs (a, sa) (b, sb) in
      bsubp big small, sbig
;;


(** 
 * Subtraction of two Bigint values, represented as lists of digits.
 * @param a  Minuend (list of int digits).
 * @param sa Minuend's sign.
 * @param b  Subtraend (list of int digits).
 * @param sb Subtraend's sign.
 * @return The a - b subtraction represented as a list of digits.
 *)
let bsub (a, sa) (b, sb) =
  bsum (a, sa) (b, inv_sign sb)
;;


(** 
 * Split a list into a couple of lists, with the first containing n elements.
 * @param n Number of elements to be contained in the first list.
 * @param l List to be split.
 * @return A couple (l1, l2) of lists such that 
 *         l = l1 @ l2
 *         List.length l = n
 *)
let split n l =
  let rec spr k a b = function
    | []     -> List.rev a, List.rev b
    | h :: t -> 
        let a, b = if k < n then h :: a, b else a, h :: b in
        spr (k + 1) a b t in 
  spr 0 [] [] l
;;


(** 
 * Product of two Bigint values, represented as lists of digits, using the
 * naive multiplication algorithm. 
 * Representing the two factors as sequences of digits
 *   a = a_m :: ... :: a_1 :: a_0 :: []
 *   b = b_n :: ... :: b_1 :: b_0 :: []
 * the product is obtained by computing
 *   {% \sum_{i=0}^n a \cdot b_i \cdot 10^i %}
 * This algorithm has a computational cost of O(n^2) digit multiplications.
 *   
 * @param a First factor (list of int digits).
 * @param sa First factor's sign.
 * @param b  Second factor (list of int digits).
 * @param sb Second factor's sign.
 * @return The a * b product represented as a list of digits.
 *)
let bmul_naive (a, sa) (b, sb) = 
  (** 
   * Compute the product of the factor `a` for the digit `y`.
   * @param r Product of the consumed digits of a.
   * @param y Digit to multiply by.
   * @param c Carry of the last digit multiplication.
   * @return The product when all the input digits have been consumed.
   *)
  let rec row r y c = function
    | []     -> List.rev (if c = 0 then r else c :: r)
    | h :: t -> 
        let p = h * y + c in
        let r, c = if p < 10 then p :: r, 0 else (p mod 10) :: r, p / 10 in
        row r y c t in 

  (** 
   * Multiply `a` for each of the digits of `b`, shifting the product and 
   * accumulating it on a sum.
   * @param k Shift amount for the current product.
   * @param r Accumulator for the sum.
   * @return The a * b product.
   *)
  let rec acc k r = function
    | []     -> r
    | h :: t -> 
        let r = fst (bsum (r, Positive) ((k @ row [] h 0 a), Positive)) in
        let k = 0 :: k in
        acc k r t in

  let sign = match sa, sb with
    | Positive, Positive
    | Negative, Negative -> Positive
    | _                  -> Negative in

  match a, b with
  | [0], _
  | [], _
  | _, [0]
  | _, []  -> [0], sign
  | [1], x
  | x, [1] -> x, sign
  | _      -> acc [] [] b, sign
;;


(** 
 * Product of two Bigint values, represented as lists of digits, using the
 * Karatsuba multiplication algorithm.
 * Representing the two factors as 
 *   {% x = x_1 * 10^m + x_2 %}
 *   {% y = y_1 * 10^m + y_2 %}
 * the product is obtained by computing
 *   {% a \cdot 10^{2m} + b \cdot 10^m + c %}
 * where
 *   {% a = x_1 \cdot y_1 %}
 *   {% c = x_2 \cdot y_2 %}
 *   {% a = (x_1 + x_2) (y_1 + y_2) %}
 * This algotithm has a computational cost of O(n^(log_2 3)) multiplications.
 *   
 * @param x  First factor (list of int digits).
 * @param sx First factor's sign.
 * @param y  Second factor (list of int digits).
 * @param sy Second factor's sign.
 * @return The a * b product represented as a list of digits.
 *)
let bmul (x, sx) (y, sy) =
  (** 
   * Below this thereshold value the naive multiplication should be convenient. 
   * This value was obtained with some rough experiments on a 
   * Intel i7 4790k machine.
   *)
  let _THERESHOLD = 55 in
  
  (** Return a list containing n zeros. *)
  let rec zeros n = if n < 1 then [] else 0 :: zeros (n - 1) in
  
  (** Recursive step of the algorithm. *)
  let rec mulr x y = 
    let nx, ny = List.length x, List.length y in
    let nmin, nmax = min nx ny, max nx ny in
    
    if nmin < _THERESHOLD then
      fst (bmul_naive (x, sx) (y, sy))
    else
      let m = nmax / 2 in 
      let (x1, x2), (y1, y2) = split m x, split m y in
      let a = mulr x2 y2 in
      let c = mulr x1 y1 in
      let b = bsubp (mulr (bsump x1 x2) (bsump y1 y2)) (bsump a c) in
      bsump (zeros (2 * m) @ a) (bsump (zeros m @ b) c) in

  let sign = match sx, sy with
    | Positive, Positive
    | Negative, Negative -> Positive
    | _                  -> Negative in

  match x, y with
  | [], _
  | [0], _
  | _, []
  | _, [0] -> [0], sign
  | [1], z
  | z, [1] -> z, sign
  | _      -> mulr x y, sign
;;


(**
 * Division of two Bigintegers using the long division algorithm.
 * @param x  Dividend (list of int digits).
 * @param sx Dividend's sign.
 * @param y  Divisor (list of int digits).
 * @param sy Divisor's sign.
 * @return A couple `(q, s), (r, s)` where `q` is the quotient, `s` is its 
 * sign and `r` is the remainder.
 *)
let bdiv_naive (x, sx) (y, sy) =
  (**
   * Division of `a` by `y` when `a` is less than ten times `y`.
   * @param a Dividend.
   * @return A couple `(q, r)` containing quotient and remainder.
   *)
  let divide a =
    let rec divr a q =
      if cmp_list a y = 1 then
        q, a
      else
        divr (bsubp a y) (q + 1) in
    divr a 0 in

  (**
   * Operate one step of the long division algorithm, consuming one digit 
   * of `x` for each iteration.
   * @param q Digits of the quotient at the current iteration.
   * @param a Dividend for the current iteration.
   * @param x Remaining digits of the input dividend to be consumed.
   * @return A couple `(q, r)` containing quotient and remainder.
   *)
  let rec divr q a x = match x with
    | []     -> zero_trim q, zero_trim a
    | h :: t ->
        let a = h :: a in
        let d, a = divide a in
        divr (d :: q) a t in
  
  let sign = match sx, sy with
    | Positive, Positive
    | Negative, Negative -> Positive
    | _                  -> Negative in
  
  let q, r = divr [] [] (List.rev x) in
  (q, sign), (r, sign)
;;


(**
 * Division of two Bigintegers.
 * @param x  Dividend (list of int digits).
 * @param sx Dividend's sign.
 * @param y  Divisor (list of int digits).
 * @param sy Divisor's sign.
 * @return A couple `(q, s)` where `q` is the quotient and `s` is its sign. 
 *)
let bdiv (x, sx) (y, sy) =
  fst (bdiv_naive (x, sx) (y, sy))
;;


(**
 * Modulus of two Bigintegers.
 * @param x  Dividend (list of int digits).
 * @param sx Dividend's sign.
 * @param y  Divisor (list of int digits).
 * @param sy Divisor's sign.
 * @return A couple `(r, s)` where `r` is the remainder and `s` is its sign. 
 *)
let bmod (x, sx) (y, sy) =
  snd (bdiv_naive (x, sx) (y, sy))
;;


(**
 * Return the semantic obtained applying an operation to two polimorfic 
 * operands.
 * @param env Environment for the computation.
 * @param a   First operand (DInt or DBigint).
 * @param b   Second operand (DInt or DBigint).
 * @param oi  Operation on integers.
 * @param ob  Operation on big-integers.
 * @return A `dval` object representing the semantic of the result.
 *)
let apply_operation a b oi ob = match a, b with
  | DInt a, DInt b -> DInt (oi a b)
  | DBigint (a, sa), DBigint (b, sb) ->
      let (q, s) = ob (a, sa) (b, sb) in
      DBigint (q, s)
  | DBigint (a, sa), DInt b ->
      let (q, s) = ob (a, sa) (cast_int b) in
      DBigint (q, s)
  | DInt a, DBigint (b, sb) ->
      let (q, s) = ob (cast_int a) (b, sb) in
      DBigint (q, s)
  | _ -> failwith "TODO artithmetic operation"
;;