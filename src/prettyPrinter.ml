open Types 

exception Fail

let rec trav_tau tau = match tau with 
| TyCon s -> (
    match s with 
      | Int -> "int"
      | Bool -> "bool"
      | String -> "string"
    )
| TyVar i -> string_of_int i
| TyFunApp {t1; t2} -> (trav_tau t1) ^ " -> " ^ (trav_tau t2)
| TyTuple {t1; t2} -> "(" ^ (trav_tau t1) ^ ")" ^" x " ^ "(" ^ (trav_tau t2) ^ ")"

(* let unfold_tyvars tyvars = List.fold_left (fun s tyvar -> " " ^ s ^ ", " ^ tyvar) "" tyvars  *)

let pretty_print_typescheme tau = 
  "Type-variables / Type-constructors in tau \n \t " ^ (trav_tau tau) ^ "\n"