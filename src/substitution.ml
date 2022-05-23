open Types

module TE = TypeEnv

module Substitution =
  Map.Make (struct 
    type t = tyvar
    let compare = compare
end)

let rec trav_tau tau =
  match tau with
  | TyCon s -> (
      match s with Int -> "int" | Bool -> "bool" | String -> "string")
  | TyVar i -> string_of_int i
  | TyFunApp { t1; t2 } -> "(" ^ trav_tau t1 ^ " -> " ^ trav_tau t2 ^ ")"
  | TyTuple { t1; t2 } ->
      "(" ^ trav_tau t1 ^ ")" ^ " x " ^ "(" ^ trav_tau t2 ^ ")"

let string_of_tau tau = trav_tau tau
let print_tau tau = print_string (string_of_tau tau ^ "\n")

type map_type = typ Substitution.t
let empty: map_type = Substitution.empty
let add k v t : map_type = Substitution.add k v t
let look_up k t: typ option = Substitution.find_opt k t
let remove k t: map_type = Substitution.remove k t 
let get_or_else k t d = 
  match look_up k t with
    | Some v -> 
      v
    | None -> 
      d
let apply t typ: typ =
  let rec find_type_rec typ' = 
    match typ' with
      | TyCon _ -> typ'
      | TyVar tv -> get_or_else tv t typ'
      | TyFunApp {t1; t2} -> TyFunApp {t1 = find_type_rec t1; t2 = find_type_rec t2}
      | TyTuple {t1; t2} -> TyTuple {t1 = find_type_rec t1; t2 = find_type_rec t2}
  in
  find_type_rec typ
let apply_to_typescheme t (TypeScheme{tyvars; tau}) = TypeScheme{tyvars; tau=apply t tau}
let apply_to_gamma t gamma = TE.map (apply_to_typescheme t) gamma
let map m (t: map_type) = Substitution.map m t
let union a b c = Substitution.union a b c
let compose (s2: map_type) (s1: map_type) = union (fun _ _ v2 -> Some v2) s2 (map (fun v -> apply s2 v) s1)
let bindings t = Substitution.bindings t
let of_seq s = Substitution.of_seq s
let of_list l = of_seq (List.to_seq l)