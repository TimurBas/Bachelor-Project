open Types
open Substitution
open TypeEnv

exception Fail

let rec trav_tau tau =
  match tau with
  | TyCon s -> (
      match s with Int -> "int" | Bool -> "bool" | String -> "string"), 0
  | TyVar i -> string_of_int i, 0
  | TyFunApp { t1; t2 } ->
      let a1, a2 = trav_tau t1 in
      let b1, b2 = trav_tau t2 in
      let string_a = if a2 > 0 then "(" ^ a1 ^ ")" else a1 in
      let string_b = if b2 > 1 then "(" ^ b1 ^ ")" else b1 in
      string_a ^ " -> " ^ string_b, 1
  | TyTuple { t1; t2 } ->
      let a1, a2 = trav_tau t1 in
      let b1, b2 = trav_tau t2 in
      let string_a = if a2 > 0 then "(" ^ a1 ^ ")" else a1 in
      let string_b = if b2 > 0 then "(" ^ b1 ^ ")" else b1 in
      string_a ^ " x " ^ string_b, 1

let string_of_tau tau = fst (trav_tau tau)
let print_tau tau = print_string (string_of_tau tau ^ "\n")

let string_of_typescheme (TypeScheme { tyvars; tau }) =
  let tyvars = String.concat ", " (List.map (fun x -> string_of_int x) (SS.elements tyvars)) in
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
