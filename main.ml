open Src

module A = Ast
module T = Types
module TE = TypeEnv
module S = Substitution
module PR = PrettyPrinter
module EX = Examples

exception Fail
exception Impossible
exception FailMessage of string

let set_difference lst1 lst2 = List.filter (fun e -> not (List.mem e lst2)) lst1

let rec find_tyvars (tau: T.typ): T.tyvar list = match tau with 
| TyCon _ -> []
| TyVar alpha -> [alpha]
| TyFunApp {t1; t2} -> (find_tyvars t1) @ (find_tyvars t2)
| TyTuple {t1; t2} -> (find_tyvars t1) @ (find_tyvars t2)

let find_free_tyvars (T.TypeScheme {tyvars; tau}) = 
  set_difference (find_tyvars tau) tyvars

let clos gamma typescheme = 
  let T.TypeScheme {tau; _} = typescheme in 
  let free_tyvars_ts = find_free_tyvars typescheme in 
  (* List.iter (fun tyvar -> print_string ("Free tyvars: " ^ (string_of_int tyvar) ^ "\n")) free_tyvars_ts; *)
  let gammas_bindings = TE.bindings gamma in 
  let gammas_typeschemes = List.map (fun (_, v) -> v) gammas_bindings in 
  let free_tyvars_gamma = List.concat_map find_free_tyvars gammas_typeschemes in 
  T.TypeScheme {tyvars = set_difference free_tyvars_ts free_tyvars_gamma; tau}

let counter = ref 0
let get_next_tyvar () = 
  counter := (!counter) + 1;
  !counter

let rec unify (t1, t2) subst: S.map_type = 
  (* print_string (PR.print_tau t1); 
  print_string (PR.print_tau t2); 
  print_newline(); *)
  match t1, t2 with 
    | T.TyCon _ , T.TyCon _ -> subst
    | T.TyFunApp {t1 = t11; t2 = t12}, T.TyFunApp{t1 = t21; t2 = t22} -> 
        S.union (fun _ a _ -> Some a) (unify (t11, t21) subst) (unify (t12, t22) subst)
    | T.TyTuple {t1 = t11; t2 = t12}, T.TyTuple {t1 = t21; t2 = t22} -> 
        S.union (fun _ a _ -> Some a) (unify (t11, t21) subst) (unify (t12, t22) subst)
    | T.TyVar ty_var1, T.TyVar ty_var2 -> 
        if ty_var1 = ty_var2 then subst else S.add ty_var1 (TyVar ty_var2) subst
    | T.TyVar ty_var, t2 -> 
        let t2_tyvars = find_tyvars t2 in 
        if List.exists (fun ty_var' -> ty_var = ty_var') t2_tyvars
          then raise (FailMessage "Recursive unification")
          else S.add ty_var t2 subst
    | t1, T.TyVar ty_var ->
      let t1_tyvars = find_tyvars t1 in 
        if List.exists (fun ty_var' -> ty_var = ty_var') t1_tyvars
          then raise (FailMessage "Recursive unification")
          else S.add ty_var t1 subst
    | _ -> raise (FailMessage "Failed unification")

let algorithm_w (exp: A.exp): S.map_type * T.typ = 
  let rec trav (gamma: TE.map_type) exp: S.map_type * T.typ =
    match exp with 
    | A.Var id -> 
      (
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
    | A.App {e1; e2} -> 
        let (s1, tau1) = trav gamma e1 in 
        let (s2, tau2) = trav (S.apply_to_gamma s1 gamma) e2 in
        let new_tyvar = get_next_tyvar() in 
        let s3 = unify (S.apply s2 tau1, T.TyFunApp{t1 = tau2; t2 = TyVar new_tyvar}) S.empty in 
        (S.compose s3 (S.compose s2 s1), S.apply s3 (TyVar new_tyvar))
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
        let (s2, tau2) = trav (S.apply_to_gamma s1 gamma) e2 in 
        (S.compose s2 s1, S.apply s2 (TyTuple {t1 = tau1; t2 = tau2}))
    | A.Fst e1 -> 
        let (s1, tau1) = trav gamma e1 in 
        (
          match tau1 with 
            | TyTuple {t1; _} -> (s1, S.apply s1 t1) 
            | _ -> raise Fail
        )
    | A.Snd e1 -> 
      let (s1, tau1) = trav gamma e1 in 
      (
        match tau1 with 
          | TyTuple {t2; _} -> (s1, S.apply s1 t2) 
          | _ -> raise Fail
      )
  in trav TE.empty exp

let () = 
  print_newline(); 
  print_string "Non_polymporphic_id_example \n";
  (* fun x -> fun y -> fun z -> z *)
  let (_, tau) = algorithm_w EX.non_polymporphic_id_example in 
  print_string (PR.print_tau tau);
  print_newline(); 
  print_string "Polymorphic_id_example \n";
  (* let id = fun x -> x in id *)
  let (_, tau) = algorithm_w EX.polymorphic_id_example in 
  print_string (PR.print_tau tau); 
  print_newline();
  print_string "Nested_let_example \n";
  (* let id = fun x -> x in let both = fun y -> fun z -> id in let amk = fun x -> both in (amk, amk) *)
  let (_, tau) = algorithm_w EX.nested_let_example in 
  print_string (PR.print_tau tau); 
  print_newline();
  print_string "Fun_application_example \n";
  (* fun x -> fun y -> x y  *)
  let (_, tau) = algorithm_w EX.fun_application_example in 
  print_string (PR.print_tau tau); 
  print_newline(); 
  print_string "Fun_application_three_example \n";
  (* fun x -> fun y -> fun z -> z (x y) *)
  let (_, tau) = algorithm_w EX.fun_application_three_example in 
  print_string (PR.print_tau tau);
  print_newline();
  print_string "Tuple_fun_application_example \n";
  (* fun x -> fun y -> (x y, x y) *)
  let (_, tau) = algorithm_w EX.tuple_fun_application_example in 
  print_string (PR.print_tau tau);
  print_newline(); 
  print_string "Lambda_outside_let_example \n";
  (* fun x -> let y = fun w -> w x in y *)
  let (_, tau) = algorithm_w EX.lambda_outside_let_example in 
  print_string (PR.print_tau tau);
  print_newline(); 
  print_string "Fst_example \n";
  (* fun x -> fst (x, x) *)
  let (_, tau) = algorithm_w EX.fst_lambda_example in 
  print_string (PR.print_tau tau);
  print_newline(); 
  print_string "Fst_let_example \n";
  (* fun x -> fun y -> let z = (y, x) in fst z *)
  let (_, tau) = algorithm_w EX.fst_let_example in 
  print_string (PR.print_tau tau);
  print_newline(); 
  print_string "Everything_example \n";
  (* fun x -> let y = fun w -> w x in fun u -> fun z -> (y u, y z) *)
  let (_, tau) = algorithm_w EX.everything_example in 
  print_string (PR.print_tau tau);