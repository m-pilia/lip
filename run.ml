(* ocamlc -c -I +camlp4 -pp camlp4of.opt main.ml *)
(* camlp4 main.cmo *)

#use "sem_static.ml";;
#use "parser.ml";;

let parse_prog s =
   Gram.parse_string exp (Loc.mk "<string>") s;;

let rec fold_lines acc = 
  try 
    let s = read_line () in fold_lines (acc ^ s ^ "\n")
  with End_of_file -> acc
;;

let _ = 
  let sprog = fold_lines "" in 
  (* to run the interpreter *)
  print_string (dump_dval (sem_exp (parse_prog sprog) (emptyenv ())))
;;

