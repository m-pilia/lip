open Camlp4.PreCast ;;

module Gram = MakeGram(Lexer) ;;

let exp_eoi = Gram.Entry.mk "exp_eoi";;
let exp  = Gram.Entry.mk "exp" ;;
let bind = Gram.Entry.mk "bind";;


EXTEND Gram

  (* Ensure that the valid expression is not followed by any trailing 
   * character.
   *)
  exp_eoi: 
  [ 
    [ e = exp; EOI -> e ] 
  ];

  (* BNF syntax for the language.
   *
   * <block>      ::= let <bind list> in <exp>
   * <bind list>  ::= <bind> [and <bind list>]
   * <bind>       ::= <liden> = <exp>
   * <int>        ::= [+-]?[0-9]+
   * <bigint>     ::= Bigint "<int>"
   * <bool>       ::= true | false
   * <liden>      ::= [a-z][a-zA-Z]*
   * <liden list> ::= [<liden> [; <liden list>]]
   * <arg list>   ::= [<exp> [; <arg list>]]
   * <cast>       ::= (big) <exp>
   * <pair>       ::= (<exp>, <exp>)
   * <list>       ::= [] | <exp> :: <list>
   * <if>         ::= if <exp> then <exp> else <exp>
   * <function>   ::= fun (<liden list>) -> <exp>
   * <apply>      ::= <exp> (<arg list>)
   *
   * <exp> ::= <block> | <int> | <bigint> | <bool> | <cast> | <pair> | <list>
   *         | <liden> | <if> | <fun> | <apply>
   *         | ( <exp> )
   *         | <exp> + <exp>
   *         | <exp> - <exp>
   *         | <exp> * <exp>
   *         | <exp> / <exp>
   *         | <exp> % <exp>
   *         | <exp> && <exp>
   *         | <exp> || <exp>
   *         | ! <exp>
   *         | ISZERO <exp>
   *         | FST <exp>
   *         | SND <exp>
   *         | HD <exp>
   *         | TL <exp>
   *)
  exp: 
  [ 
    "equals" LEFTA
    [ e1 = exp; "="; e2 = exp -> Eq(e1,e2) 
    | e1 = exp; "<"; e2 = exp -> Less(e1,e2) ]
  | "Sum,Diff" LEFTA
    [ e1 = exp; "+"; e2 = exp -> Sum(e1,e2) 
    | e1 = exp; "-"; e2 = exp -> Diff(e1,e2) 
    | "-"; e = exp -> Diff(Eint 0, e)]
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
  | "Head,Tail" RIGHTA
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
    | "Bigint"; i = STRING -> str_to_bigint i
    | "true"  -> Ebool(true)
    | "false" -> Ebool(false)
    | "("; e = SELF; ")" -> e 
    | "["; "]" -> Emptylist
    | "["; l = LIST1 SELF SEP ";"; "]" -> cons_of_list l
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
