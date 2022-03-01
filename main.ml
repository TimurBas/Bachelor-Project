open Src

module A = Ast
module T = Types
module TE = TypeEnv
module S = Substitution
module PR = PrettyPrinter

exception Fail
exception Impossible
exception FailMessage of string

let non_polymporphic_id_example = A.Lambda {id = "x"; e1 = A.Var "x"}

let polymorphic_id_example = A.Let {id = "id"; 
                                   e1 = A.Lambda {id = "x"; e1 = A.Var "x"};
                                   e2 = A.Var "id"}
                     
let init_tyvars: string list =
[
   "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h";  "i"; "j"; "k"; "l"; "m";
   "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u";  "v"; "w"; "x"; "y"; "z"
]

let remove_tyvar (tyvars: string list) (tyvar: string) = 
  let filtered_tyvars = List.filter (fun c -> c = tyvar) tyvars in 
  if filtered_tyvars = tyvars then raise (FailMessage "could not remove tyvar because it didn't exist") else filtered_tyvars

let get_tyvar (tyvars: string list) = 
  List.hd tyvars
  (* match tyvars with 
| x::_ -> x
| [] -> raise (FailMessage "get_tyvar failed because tyvars list is empty") *)

let map_tyvar_to_new_tyvar tyvars old_tyvar = 
  let updated_tyvars = remove_tyvar tyvars old_tyvar in 
  get_tyvar updated_tyvars

let wrap_monotype tau = T.TypeScheme {tyvars=[]; tau}

let set_difference lst1 lst2 = List.filter (fun e -> not (List.mem e lst2)) lst1

let rec find_tyvars (tau: T.typ): string list = match tau with 
| TyCon pi -> [pi] 
| TyVar alpha -> [alpha]
| TyFunApp {t1; t2} -> (find_tyvars t1) @ (find_tyvars t2)
| TyTuple {t1; t2} -> (find_tyvars t1) @ (find_tyvars t2)

let find_free_tyvars (T.TypeScheme {tyvars; tau}) = 
  set_difference tyvars (find_tyvars tau)

let clos gamma typescheme = 
  let T.TypeScheme {tau; _} = typescheme in 
  let free_tyvars_ts = find_free_tyvars typescheme in 
  let gammas_bindings = TE.bindings gamma in 
  let gammas_typeschemes = List.map (fun (_, v) -> v) gammas_bindings in 
  let free_tyvars_gamma = List.concat_map find_free_tyvars gammas_typeschemes in 
  T.TypeScheme {tyvars = set_difference free_tyvars_ts free_tyvars_gamma; tau}

let algorithm_w (exp: A.exp): S.map_type * T.typescheme = 
  let tyvars' = init_tyvars in 
  let rec trav (gamma: TE.map_type) exp =
    match exp with 
    | A.Var id -> (
        match TE.look_up id gamma with 
          | Some typescheme -> 
            let TypeScheme{tyvars; tau} = typescheme in
            (
              match tyvars with 
                | [] -> (S.empty, wrap_monotype tau)
                | tyvars -> let new_tyvars = List.map (map_tyvar_to_new_tyvar tyvars') tyvars in
                  (S.empty, TypeScheme {tyvars = new_tyvars; tau})
            )
          | None -> raise Fail
    )
    | A.Lambda {id; e1} -> 
        let new_tyvar = get_tyvar tyvars' in
        let (s, typescheme) = trav (TE.add id (wrap_monotype (TyVar new_tyvar)) gamma) e1 in 
        let TypeScheme {tau; _} = typescheme in 
        let tau' = 
          match S.look_up new_tyvar s with 
            | Some typ -> typ
            | None -> T.TyVar new_tyvar
        in 
        (s, wrap_monotype (T.TyFunApp {t1 = tau'; t2 = tau}))
    (* | A.App {e1; e2} -> raise Fail *)
    | A.Let {id; e1; e2} -> 
        let (s1, tau1) = trav gamma e1 in 
        let (s2, tau2) = trav (TE.add id (clos () tau1)) e2 in 
        (s2, tau2)
    | _ -> raise Fail
in trav TE.empty exp

let () = 
  let (_, typescheme) = algorithm_w non_polymporphic_id_example in 
  print_string (PR.pretty_print_typescheme typescheme)