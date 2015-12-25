open Camlp4.PreCast ;;

module Gram = MakeGram(Lexer) ;;

let exp  = Gram.Entry.mk "exp" ;;
let bind = Gram.Entry.mk "bind";;

EXTEND Gram

  exp: 
  [ 
    "equals" LEFTA
    [ e1 = exp; "="; e2 = exp -> Eq(e1,e2) 
    | e1 = exp; "<"; e2 = exp -> Less(e1,e2) ]
  | "Sum,Diff" LEFTA
    [ e1 = exp; "+"; e2 = exp -> Sum(e1,e2) 
    | e1 = exp; "-"; e2 = exp -> Diff(e1,e2) ]
  | "And,Or" LEFTA
    [ e1 = exp; "&&"; e2 = exp -> And(e1,e2) 
    | e1 = exp; "||"; e2 = exp ->  Or(e1,e2) ]
  | "Prod,Div,Mod" LEFTA
    [ e1 = exp; "*"; e2 = exp -> Prod(e1,e2) 
    | e1 = exp; "/"; e2 = exp -> Div(e1,e2) 
    | e1 = exp; "%"; e2 = exp -> Mod(e1,e2) ]
  | "Castint" RIGHTA
    [ "(big)"; e = SELF -> Castint(e) ]
  | "Cons" RIGHTA
    [ e1 = exp; "::"; e2 = exp -> Cons(e1,e2)]
  | "Head,Tail" LEFTA
    [ "HD"; e = exp -> Head(e)
    | "TL"; e = exp -> Tail(e)]
  | "Not" RIGHTA
    [ "!"; e = exp -> Not(e) ]
  | "Iszero" RIGHTA
    [ "ISZERO"; e = exp -> Iszero(e)]
  | "Fst,Snd" RIGHTA
    [ "FST"; e = exp -> Fst(e)
    | "SND"; e = exp -> Snd(e)]
  | "Values"
    [ `INT(i,_) -> Eint(i) 
    | `LIDENT x -> Den(x) 
    | "Bigint"; `INT(i,_) -> Bigint (fst(cast_int i))
    | "true"  -> Ebool(true)
    | "false" -> Ebool(false)
    | "("; e = SELF; ")" -> e 
    | "[]" -> Emptylist
    | "("; e1 = exp; ","; e2 = exp; ")" -> Pair(e1,e2)]
  | "Ifthenelse" LEFTA
    [ "if"; e = exp; "then"; c1 = SELF; "else"; c2 = SELF 
        -> Ifthenelse(e,c1,c2)]
  | "Let" LEFTA
    [ "let"; l = LIST1 bind SEP "and"; "in"; e = SELF 
        -> Let(l,e)]
  | "Fun" LEFTA
    [ "fun"; "("; l = LIST0 [`LIDENT x -> x] SEP ";"; ")"; "->"; e = SELF 
        -> Fun(l, e)]
  | "Apply" RIGHTA
    [ e = exp; "("; l = LIST0 SELF SEP ";"; ")" -> Apply(e, l) ]
  ];

  bind:
  [
    [`LIDENT x; "="; v = exp -> (x, v)]
  ];

END ;;
