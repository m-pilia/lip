open Camlp4.PreCast ;;

module Gram = MakeGram(Lexer) ;;

let exp  = Gram.Entry.mk "exp" ;;

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
  | "Prod" LEFTA
    [ e1 = exp; "*"; e2 = exp -> Prod(e1,e2) ]
  | "Not" RIGHTA
    [ "!"; e = exp -> Not(e) ]
  | "Values"
    [ `INT(i,_) -> Eint(i) 
    | `LIDENT x -> Den(x) 
    | "("; e = SELF; ")" -> e ]
  | "Ifthenelse" LEFTA
    ["if"; e = exp; "then"; c1 = SELF; "else"; c2 = SELF -> Ifthenelse(e,c1,c2)]
  | "Let" LEFTA
    ["let"; `LIDENT x; "="; v = SELF; "in"; e = SELF -> Let([x, v],e)]
  ];

END ;;
