open Types

module TE = TypeEnv

let new_tyvar () = TyVar (ref (Int (TE.get_next_tyvar())))
let ( ~$ ) t = find t
let ( +- ) gamma (id, ts) = TE.add id ts gamma
let ( !& ) t = TE.wrap_monotype t
let ( => ) t1 t2 = TyFunApp { t1; t2 }
let ( ** ) t1 t2 = TyTuple { t1; t2 }

let combine_sets sets = List.fold_left (fun a b -> SS.union a b) SS.empty sets

let assoc_or_else bindings key ~default = Option.value (List.assoc_opt key bindings) ~default:default