open Types

module UF = UnionFind
module TE = TypeEnv

let new_node () = UF.make_set (TyVar (TE.get_next_tyvar()))
let ( ~% ) tau_node = UF.root tau_node
let ( ~& ) gamma = TE.update_typeschemes gamma
let ( +- ) gamma (id, ts) = TE.add id ts gamma
let ( !$ ) t = TE.wrap_monotype t
let ( => ) t1 t2 = UF.make_set (TyFunApp { t1; t2 })
let ( ** ) t1 t2 = UF.make_set (TyTuple { t1; t2 })

let list_of_set set = List.of_seq (SS.to_seq set)
let combine_sets sets = List.fold_left (fun a b -> SS.union a b) SS.empty sets