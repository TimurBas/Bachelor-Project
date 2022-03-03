open Src

module A = Ast
module T = Types
module TE = TypeEnv
module S = Substitution
module PR = PrettyPrinter

exception Fail
exception Impossible
exception FailMessage of string
let non_polymporphic_id_example = A.Lambda {id = "x"; e1 = A.Lambda {id = "y"; e1 = A.Lambda {id = "z"; e1 = Var "z"}}}

let polymorphic_id_example = A.Let {id = "id"; 
                                   e1 = A.Lambda {id = "x"; e1 = A.Var "x"};
                                   e2 = A.Var "id"}
                     
(* let init_tyvars: string list =
[
   "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m";
   "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z"
]

let remove_tyvar tyvars tyvar = 
  let filtered_tyvars = List.filter (fun c -> not(c = tyvar)) !tyvars in 
  if filtered_tyvars = !tyvars then raise (FailMessage "could not remove tyvar because it didn't exist") else tyvars := filtered_tyvars

let get_tyvar (tyvars: string list) = 
  List.hd tyvars *)

(* let map_tyvar_to_new_tyvar tyvars old_tyvar = 
  remove_tyvar tyvars old_tyvar; 
  get_tyvar !tyvars *)

let set_difference lst1 lst2 = List.filter (fun e -> not (List.mem e lst2)) lst1

let rec find_tyvars (tau: T.typ): T.tyvar list = match tau with 
| TyCon _ -> []
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

let counter = ref 0
let get_next_tyvar () = 
  counter := (!counter) + 1;
  !counter

let algorithm_w (exp: A.exp): S.map_type * T.typ = 
  let rec trav (gamma: TE.map_type) exp: S.map_type * T.typ =
    match exp with 
    | A.Var id -> (
        match TE.look_up id gamma with 
          | Some TypeScheme{tyvars; tau} -> 
            let new_tyvars = List.map (fun _ -> get_next_tyvar()) tyvars in
            let new_subst = 
              List.fold_left2 (fun s new_tv tv ->
                S.add tv (T.TyVar new_tv) s) S.empty new_tyvars tyvars in 
            (S.empty, S.apply new_subst tau)
          | None -> raise Fail
    )
    | A.Lambda {id; e1} -> 
        let new_tyvar = get_next_tyvar() in
        let (s, tau) = trav (TE.add_alpha id (TyVar new_tyvar) gamma) e1 in 
        let tau' = 
          match S.look_up new_tyvar s with 
            | Some typ -> typ
            | None -> T.TyVar new_tyvar
        in 
        (s, T.TyFunApp {t1 = tau'; t2 = tau})
    (* | A.App {e1; e2} -> raise Fail *)
    (* | A.Let {id; e1; e2} -> 
        let (s1, tau1) = trav gamma e1 in 
        let (s2, tau2) = trav (TE.add id (clos () tau1)) e2 in 
        (s2, tau2) *)
    | _ -> raise Fail
in trav TE.empty exp

let () = 
  let (_, tau) = algorithm_w non_polymporphic_id_example in 
  print_string (PR.pretty_print_typescheme tau)