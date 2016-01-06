#use "parser/parser.ml";;

let parse_prog s =
   Gram.parse_string exp_eoi (Loc.mk "<string>") s;;

let rec fold_lines acc =
  try
    let s = read_line () in fold_lines (acc ^ s ^ "\n")
  with End_of_file -> acc
;;

let _ =
  let sprog = fold_lines "" in
  print_string (dump_dval (sem_exp (parse_prog sprog) (emptyenv ())) ^ "\n")
;;
