open Types

module TE = TypeEnv

module Substitution =
  Map.Make (struct 
    type t = tyvar
    let compare = compare
end)

type map_type = typ Substitution.t
let empty: map_type = Substitution.empty
let add k v t : map_type = Substitution.add k v t
let look_up k t: typ option = Substitution.find_opt k t
let remove k t: map_type = Substitution.remove k t 
let get_or_else k t ~default = 
  match look_up k t with
    | Some v -> v
    | None -> default
let apply t typ: typ =
  let rec subst typ' = 
    match typ' with
      | TyCon _ -> typ'
      | TyVar tv -> get_or_else tv t ~default:typ'
      | TyFunApp {t1; t2} -> TyFunApp {t1 = subst t1; t2 = subst t2}
      | TyTuple {t1; t2} -> TyTuple {t1 = subst t1; t2 = subst t2}
  in
  subst typ
let apply_to_typescheme t (TypeScheme{tyvars; tau}) = TypeScheme{tyvars; tau=apply t tau}
let apply_to_gamma t gamma = TE.map (apply_to_typescheme t) gamma
let map m (t: map_type) = Substitution.map m t
let union a b c = Substitution.union a b c
let compose (s2: map_type) (s1: map_type) = union (fun _ _ v2 -> Some v2) s2 (map (fun v -> apply s2 v) s1)
let bindings t = Substitution.bindings t
let of_seq s = Substitution.of_seq s
let of_list l = of_seq (List.to_seq l)