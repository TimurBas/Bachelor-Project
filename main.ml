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

let remove_elt e l =
  let rec remove l acc = match l with
    | [] -> List.rev acc
    | x::xs when e = x -> remove xs acc
    | x::xs -> remove xs (x::acc)
  in remove l []

let remove_duplicates lst = 
  let rec remove l acc = match l with
    | [] -> List.rev acc
    | x :: xs -> remove (remove_elt x xs) (x::acc)
  in remove lst []

let set_difference lst1 lst2 = List.filter (fun e -> not (List.mem e lst2)) lst1

let rec find_tyvars (tau : T.typ) : T.tyvar list =
  match tau with
  | TyCon _ -> []
  | TyVar alpha -> [ alpha ]
  | TyFunApp { t1; t2 } -> find_tyvars t1 @ find_tyvars t2
  | TyTuple { t1; t2 } -> find_tyvars t1 @ find_tyvars t2

let find_free_tyvars (T.TypeScheme { tyvars; tau }) =
  remove_duplicates (set_difference (find_tyvars tau) tyvars)

let clos gamma typescheme =
  let (T.TypeScheme { tau; _ }) = typescheme in
  let free_tyvars_ts = find_free_tyvars typescheme in
  List.iter
    (fun tyvar ->
      print_string ("Free tyvars typescheme: " ^ string_of_int tyvar ^ "\n"))
    free_tyvars_ts;
  let gammas_bindings = TE.bindings gamma in
  let gammas_typeschemes = List.map (fun (_, v) -> v) gammas_bindings in
  let free_tyvars_gamma = List.concat_map find_free_tyvars gammas_typeschemes in
  List.iter
    (fun tyvar ->
      print_string ("Free tyvars gamma: " ^ string_of_int tyvar ^ "\n"))
    free_tyvars_ts;
  let diff = set_difference free_tyvars_ts free_tyvars_gamma in 
  print_string "Set difference: {";
  List.iter (fun tyvar -> print_string (string_of_int tyvar ^ ", ")) diff;
  print_string "}\n";
  T.TypeScheme { tyvars = diff; tau }

let counter = ref 0

let get_next_tyvar () =
  counter := !counter + 1;
  !counter

let rec unify (t1, t2) subst : S.map_type =
  (* print_string (PR.string_of_tau t1);
     print_string (PR.string_of_tau t2);
     print_newline(); *)
  match (t1, t2) with
  | T.TyVar tyvar, T.TyCon b -> S.add tyvar (TyCon b) subst
  | T.TyCon b, T.TyVar tyvar -> S.add tyvar (TyCon b) subst
  | T.TyCon _, T.TyCon _ -> subst
  | T.TyFunApp { t1 = t11; t2 = t12 }, T.TyFunApp { t1 = t21; t2 = t22 } ->
      let s1 = unify (t11, t21) subst in 
      let s2 = unify (S.apply s1 t12, S.apply s1 t22) subst in 
      S.union
      (fun _ a _  -> Some a)
      s2
      s1
  | T.TyTuple { t1 = t11; t2 = t12 }, T.TyTuple { t1 = t21; t2 = t22 } ->
      let s1 = unify (t11, t21) subst in 
      let s2 = unify (S.apply s1 t12, S.apply s1 t22) s1 in 
      S.union
      (fun _ a _  -> Some a)
      s2
      s1
  | T.TyVar ty_var1, T.TyVar ty_var2 ->
      if ty_var1 = ty_var2 then subst else S.add ty_var1 (TyVar ty_var2) subst
  | T.TyVar ty_var, t2 ->
      let t2_tyvars = find_tyvars t2 in
      if List.exists (fun ty_var' -> ty_var = ty_var') t2_tyvars then
        raise (FailMessage "Recursive unification")
      else S.add ty_var t2 subst
  | t1, T.TyVar ty_var ->
      let t1_tyvars = find_tyvars t1 in
      if List.exists (fun ty_var' -> ty_var = ty_var') t1_tyvars then
        raise (FailMessage "Recursive unification")
      else S.add ty_var t1 subst
  | _ -> raise (FailMessage "Failed unification")

let algorithm_w (exp : A.exp) : S.map_type * T.typ =
  let rec trav (gamma : TE.map_type) exp : S.map_type * T.typ =
    match exp with
    | A.Var id -> (
        match TE.look_up id gamma with
        | Some (TypeScheme { tyvars; tau }) ->
            let new_tyvars = List.map (fun _ -> get_next_tyvar ()) tyvars in
            let new_subst =
              List.fold_left2
                (fun s new_tv tv -> S.add tv (T.TyVar new_tv) s)
                S.empty new_tyvars tyvars
            in
            (S.empty, S.apply new_subst tau)
        | None -> raise Fail)
    | A.Lambda { id; e1 } ->
        let new_tyvar = get_next_tyvar () in
        let s, tau = trav (TE.add_alpha id (TyVar new_tyvar) gamma) e1 in
        let tau' =
          match S.look_up new_tyvar s with
          | Some typ -> typ
          | None -> T.TyVar new_tyvar
        in
        (s, TyFunApp { t1 = tau'; t2 = tau })
    | A.App { e1; e2 } ->
        let s1, tau1 = trav gamma e1 in
        let s2, tau2 = trav (S.apply_to_gamma s1 gamma) e2 in
        let new_tyvar = get_next_tyvar () in
        PR.print_substitution s2;
        print_string ("Type-variable in app: " ^ string_of_int new_tyvar ^ "\n");
        print_string ("Tau1 in app: " ^ PR.string_of_tau tau1 ^ "\n");
        print_string ("Tau2 in app: " ^ PR.string_of_tau tau2 ^ "\n");
        let s3 =
          unify
            (S.apply s2 tau1, T.TyFunApp { t1 = tau2; t2 = TyVar new_tyvar }) S.empty
        in
        PR.print_gamma gamma;
        PR.print_substitution s3;
        (S.compose s3 (S.compose s2 s1), S.apply s3 (TyVar new_tyvar))
    | A.Let { id; e1; e2 } ->
        let s1, tau1 = trav gamma e1 in
        let s1_gamma = S.apply_to_gamma s1 gamma in
        let clos_s1_gamma_tau = clos s1_gamma (TE.wrap_monotype tau1) in
        let gamma_ext = TE.add id clos_s1_gamma_tau gamma in
        let s1_gamma_ext = S.apply_to_gamma s1 gamma_ext in
        let s2, tau2 = trav s1_gamma_ext e2 in
        (S.compose s2 s1, tau2)
    | A.Tuple { e1; e2 } ->
        let s1, tau1 = trav gamma e1 in
        let s2, tau2 = trav (S.apply_to_gamma s1 gamma) e2 in
        (S.compose s2 s1, S.apply s2 (TyTuple { t1 = tau1; t2 = tau2 }))
    | A.Fst e1 -> (
        let s1, tau1 = trav gamma e1 in
        match tau1 with TyTuple { t1; _ } -> (s1, t1) | _ -> raise Fail)
    | A.Snd e1 -> (
        let s1, tau1 = trav gamma e1 in
        match tau1 with TyTuple { t2; _ } -> (s1, t2) | _ -> raise Fail)
    | A.BasVal b -> (
        let empty_map = S.empty in
        match b with
        | Int _ -> (empty_map, TyCon Int)
        | Bool _ -> (empty_map, TyCon Bool)
        | String _ -> (empty_map, TyCon String))
  in
  trav TE.empty exp

let run_example ast name =
  print_newline ();
  print_string name;
  let _, tau = algorithm_w ast in
  print_string (PR.string_of_tau tau);
  counter := 0;
  print_newline ()

let () =
  (* fun x -> fun y -> fun z -> z *)
  run_example EX.non_polymporphic_id_example "Non_polymporphic_id_example \n";
  (* let id = fun x -> x in id *)
  run_example EX.polymorphic_id_example "Polymorphic_id_example \n";
  (* let id = fun x -> x in let both = fun y -> fun z -> id in let amk = fun x -> both in (amk, amk) *)
  run_example EX.nested_let_example "Nested_let_example \n";
  (* fun x -> fun y -> x y  *)
  run_example EX.fun_application_example "Fun_application_example \n";
  (* fun x -> fun y -> fun z -> z (x y) *)
  run_example EX.fun_application_three_example
    "Fun_application_three_example \n";
  (* fun x -> fun y -> (x y, x y) *)
  run_example EX.tuple_fun_application_example
    "Tuple_fun_application_example \n";
  (* fun x -> let y = fun w -> w x in y *)
  run_example EX.lambda_outside_let_example "Lambda_outside_let_example \n";
  (* fun x -> fst (x, x) *)
  run_example EX.fst_lambda_example "Fst_lambda_example \n";
  (* fun x -> fun y -> let z = (y, x) in fst z *)
  run_example EX.fst_let_example "Fst_let_example \n";
  (* fun x -> let y = fun w -> w x in fun u -> fun z -> (y u, y z) *)
  run_example EX.everything_example "Everything_example \n";
  (*
     let id = fun x -> x in
     let both = (id 2, id true) in
     both
  *)
  run_example EX.polymorphic_id_with_int_and_bool
    "Polymorphic_id_with_int_and_bool \n";
  run_example EX.debug_example "debug \n"
