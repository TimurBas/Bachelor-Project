open Types
open TypeEnv
open Utils

let string_of_tau tau_node =
  let rec trav tau = 
    match ~$tau with
    | TyCon s -> (
        match s with Int -> "int" | Bool -> "bool" | String -> "string"), 0
    | TyVar {contents = Int i} -> string_of_int i, 0
    | TyVar _ -> raise (Fail "string_of_tau tyvar link")
    | TyFunApp { t1; t2 } ->
        let a1, a2 = trav t1 in
        let b1, b2 = trav t2 in
        let string_a = if a2 > 0 then "(" ^ a1 ^ ")" else a1 in
        let string_b = if b2 > 1 then "(" ^ b1 ^ ")" else b1 in
        string_a ^ " -> " ^ string_b, 1
    | TyTuple { t1; t2 } ->
        let a1, a2 = trav t1 in
        let b1, b2 = trav t2 in
        let string_a = if a2 > 0 then "(" ^ a1 ^ ")" else a1 in
        let string_b = if b2 > 0 then "(" ^ b1 ^ ")" else b1 in
        string_a ^ " x " ^ string_b, 1
  in
  fst (trav tau_node)

let print_tau_node tau = print_string (string_of_tau tau ^ "\n")

let string_of_typescheme (TypeScheme { tyvars; tau }) =
  let tyvars = String.concat ", " (List.map (fun x -> string_of_int x) (SS.elements tyvars)) in
  "∀ " ^ tyvars ^ " . " ^ (string_of_tau tau)
let print_typescheme typescheme = print_string (string_of_typescheme typescheme ^ "\n")

let string_of_tyvars tyvars = 
  let elems = String.concat ", " (List.map (fun x -> string_of_int x) tyvars) in
  "{ " ^ elems ^ " }"
let print_tyvars tyvars = print_string (string_of_tyvars tyvars ^ "\n")

let string_of_gamma gamma = 
  let elems = String.concat ", " (List.map (fun (k, v) -> k ^ " -> " ^ string_of_typescheme v) (Gamma.bindings gamma)) in
  "{ " ^ elems ^ " }"
let print_gamma gamma = print_string (string_of_gamma gamma ^ "\n")