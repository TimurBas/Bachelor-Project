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

let let_example = 
  A.Let {
  id = "id";
  e1 = A.Lambda{id = "x"; e1 = A.Var "x"};
  e2 = 
    A.Let{
    id = "both";
    e1 =A.Lambda{id = "y"; e1 = A.Lambda{id="z"; e1= A.Var "id"}}; 
    e2= A.Let{
      id = "amk"; 
      e1 = A.Lambda{id="x"; e1 = A.Var "both"};
      e2 = A.Tuple{e1=A.Var "amk"; e2=A.Var "amk"}}}}

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

(* let unify ((typ: T.typ), (T.TyFunApp{t1; t2})): S.map_type = raise Fail *)

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
        (s, TyFunApp {t1 = tau'; t2 = tau})
    (* | A.App {e1; e2} -> 
        let (s1, tau1) = trav gamma e1 in 
        let (s2, tau2) = trav (S.apply_to_gamma s1 gamma) e2 in
        let new_tyvar = get_next_tyvar() in 
        let s3 = unify (S.apply s2 tau1, T.TyFunApp{t1=tau2; t2=TyVar new_tyvar}) in  
        raise Fail *)
    | A.Let {id; e1; e2} -> 
        let (s1, tau1) = trav gamma e1 in 
        let s1_gamma = S.apply_to_gamma s1 gamma in
        let clos_s1_gamma_tau = clos s1_gamma (TE.wrap_monotype tau1) in 
        let gamma_ext = TE.add id clos_s1_gamma_tau gamma in
        let s1_gamma_ext = S.apply_to_gamma s1 gamma_ext in
        let (s2, tau2) = trav s1_gamma_ext e2 in 
        (S.compose s2 s1, tau2)
    | A.Tuple {e1; e2} -> 
        let (s1, tau1) = trav gamma e1 in 
        let (s2, tau2) = trav gamma e2 in 
        (S.compose s2 s1, TyTuple {t1 = tau1; t2 = tau2})
    | _ -> raise Fail
  in trav TE.empty exp


let () = 
  print_newline(); 
  print_string "Let_example \n";
  let (_, tau) = algorithm_w let_example in 
  print_string (PR.print_tau tau); 
  print_newline();
  print_string "Non_polymporphic_id_example \n";
  let (_, tau) = algorithm_w non_polymporphic_id_example in 
  print_string (PR.print_tau tau);
  print_newline(); 
  print_string "Polymorphic_id_example \n";
  let (_, tau) = algorithm_w polymorphic_id_example in 
  print_string (PR.print_tau tau); 
  print_newline(); 