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
let get_or_else k t d = 
  match look_up k t with
    | Some v -> v
    | None -> d
let apply t typ: typ =
  let rec find_type_rec typ' = 
    match typ' with
      | TyCon _ -> typ'
      | TyVar tv -> get_or_else tv t typ'
      | TyFunApp {t1; t2} -> TyFunApp {t1=find_type_rec t1; t2=find_type_rec t2}
      | TyTuple {t1; t2} -> TyTuple {t1=find_type_rec t1; t2=find_type_rec t2}
  in
  find_type_rec typ

let apply_to_typescheme t (TypeScheme{tyvars; tau}) = TypeScheme{tyvars; tau=apply t tau}

let apply_to_gamma t gamma = TE.map (apply_to_typescheme t) gamma

(*S: TyVar -> Type*)
let map m (t: map_type) = Substitution.map m t
let compose (s1: map_type) (s2: map_type) = map (fun v -> apply s2 v) s1

  (*         (
          let new_typ = look_up tv t in
          match new_typ with 
            | Some new_typ' -> [new_typ']
            | None -> [typ']
        )
  *)



(* let merge lst1 lst2: map_type = Substitution.merge 
(
  fun k xo yo -> 
    match xo, yo with 
      | Some x, Some y -> Some (x+y)
      | None, yo -> yo
      | xo, None -> xo
) lst1 lst2 *)