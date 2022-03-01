open Types 

exception Fail

let rec trav_tau tau = match tau with 
| TyCon s -> s
| TyVar s -> s 
| TyFunApp {t1; t2} -> (trav_tau t1) ^ " -> " ^ (trav_tau t2)
| TyTuple {t1; t2} -> (trav_tau t1) ^ " x " ^ (trav_tau t2)

let unfold_tyvars tyvars = List.fold_left (fun s tyvar -> " " ^ s ^ ", " ^ tyvar) "" tyvars 

let pretty_print_typescheme typescheme = 
  let TypeScheme {tyvars; tau} = typescheme in 
  "Bound type-variables \n \t ∀" ^ (unfold_tyvars tyvars) ^
  " \nType-variables / Type-constructors in tau \n \t " ^ (trav_tau tau) ^ "\n"