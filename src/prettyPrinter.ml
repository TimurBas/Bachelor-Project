open Types
open Substitution
open TypeEnv

exception Fail

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

let string_of_typescheme (TypeScheme { tyvars; tau }) =
  let tyvars = String.concat ", " (List.map (fun x -> string_of_int x) tyvars) in
  "∀ " ^ tyvars ^ ". " ^ (string_of_tau tau)
let print_typescheme typescheme = print_string (string_of_typescheme typescheme ^ "\n")

let string_of_tyvars tyvars = 
  let elems = String.concat ", " (List.map (fun x -> string_of_int x) tyvars) in
  "{" ^ elems ^ "}"
let print_tyvars msg tyvars = print_string (msg ^ string_of_tyvars tyvars ^ "\n")

let string_of_gamma gamma = 
  let elems = String.concat ", " (List.map (fun (k, v) -> k ^ " -> " ^ string_of_typescheme v) (Gamma.bindings gamma)) in
  "{" ^ elems ^ "}"
let print_gamma gamma = print_string (string_of_gamma gamma ^ "\n")

let string_of_substitution subst = 
  let elems = String.concat ", " (List.map (fun (k, v) -> string_of_int k ^ " -> " ^ string_of_tau v) (Substitution.bindings subst)) in
  "{" ^ elems ^ "}"
let print_substitution subst = print_string (string_of_substitution subst ^ "\n")
