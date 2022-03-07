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
  PR.print_tyvars "Free tyvars typescheme: " free_tyvars_ts;
  let gammas_bindings = TE.bindings gamma in
  let gammas_typeschemes = List.map (fun (_, v) -> v) gammas_bindings in
  let free_tyvars_gamma = List.concat_map find_free_tyvars gammas_typeschemes in
  PR.print_tyvars "Free tyvars gamma: " free_tyvars_gamma;
  let diff = set_difference free_tyvars_ts free_tyvars_gamma in 
  PR.print_tyvars "Set difference: " diff;
  T.TypeScheme { tyvars = diff; tau }

let occurs_check ty_var tau = 
  let free_tyvars = find_free_tyvars (TE.wrap_monotype tau) in 
  if List.mem ty_var free_tyvars then raise (FailMessage "Recursive unification")

let counter = ref 0

let get_next_tyvar () =
  counter := !counter + 1;
  !counter

let rec unify t1 t2 = 
  match t1, t2 with 
  | T.TyVar ty_var1, T.TyVar ty_var2 when ty_var1 = ty_var2 -> S.empty
  | T.TyVar ty_var1, T.TyVar ty_var2 when ty_var1 != ty_var2 -> S.add ty_var1 (TyVar ty_var2) S.empty
  | T.TyVar ty_var, t
  | t, T.TyVar ty_var -> occurs_check ty_var t; S.add ty_var t S.empty
  | T.TyFunApp { t1 = t11; t2 = t12 }, T.TyFunApp { t1 = t21; t2 = t22 } 
  | T.TyTuple { t1 = t11; t2 = t12 }, T.TyTuple { t1 = t21; t2 = t22 } ->
    let s1 = unify t11 t21 in 
    let s2 = unify (S.apply s1 t12) (S.apply s1 t22) in 
    S.compose s2 s1
  | T.TyCon c1, T.TyCon c2 when c1 = c2 -> S.empty
  | _ -> raise Fail

let algorithm_w (exp : A.exp) : S.map_type * T.typ =
  let rec trav (gamma : TE.map_type) exp : S.map_type * T.typ =
    match exp with
    | A.Var id -> (
        match TE.look_up id gamma with
        | Some (TypeScheme { tyvars; tau }) ->
            print_string ("Var case with id " ^ id ^ "\n");
            PR.print_tyvars "Typescheme tyvars: " tyvars;
            let new_tyvars = List.map (fun _ -> get_next_tyvar ()) tyvars in
            PR.print_tyvars "New tyvars: " new_tyvars;
            let new_subst =
              List.fold_left2
                (fun s new_tv tv -> S.add tv (T.TyVar new_tv) s)
                S.empty new_tyvars tyvars
            in
            print_string "New_subst: ";
            PR.print_substitution new_subst;
            print_string "Tau is: ";
            PR.print_tau tau;
            print_string "Applying new_subst on tau: ";
            let applied_subs_on_tau = S.apply new_subst tau in 
            PR.print_tau applied_subs_on_tau;
            print_newline();
            (S.empty, applied_subs_on_tau)
        | None -> raise Fail)
    | A.Lambda { id; e1 } ->
        print_string ("Lambda case with id " ^ id ^ "\n");
        let new_tyvar = get_next_tyvar () in
        PR.print_tyvars "New tyvar: " [new_tyvar];
        print_string "Extending gamma: ";
        let gamma_ext = TE.add_alpha id (TyVar new_tyvar) gamma in 
        PR.print_gamma gamma_ext;
        print_string "Traversing e1 \n \n";
        let s, tau = trav gamma_ext e1 in
        print_string "Lambda e1's substitution: ";
        PR.print_substitution s;
        print_string "Lambda e1's tau: ";
        PR.print_tau tau;
        print_string ("Lambda look up S(" ^ (string_of_int new_tyvar) ^ "): ");
        let tau' =
          match S.look_up new_tyvar s with
          | Some typ -> typ
          | None -> T.TyVar new_tyvar
        in
        PR.print_tau tau';
        print_newline();
        (s, TyFunApp { t1 = tau'; t2 = tau })
    | A.App { e1; e2 } ->
        print_string "App case \n";
        print_string "Traversing e1 \n \n";
        let s1, tau1 = trav gamma e1 in
        print_string "App e1's substitution: ";
        PR.print_substitution s1;
        print_string "App e1's tau: ";
        PR.print_tau tau1;
        let s1_applied_to_gamma = (S.apply_to_gamma s1 gamma) in 
        print_string "S1 \\gamma: ";
        PR.print_gamma s1_applied_to_gamma;
        print_string "Traversing e2 \n \n";
        let s2, tau2 = trav s1_applied_to_gamma e2 in
        print_string "App e2's substitution: ";
        PR.print_substitution s2;
        print_string "App e2's tau: ";
        PR.print_tau tau2;
        let new_tyvar = get_next_tyvar () in
        PR.print_tyvars "New tyvar: " [new_tyvar];
        let apply_s2_tau1 = S.apply s2 tau1 in 
        print_string "S2 tau1: ";
        PR.print_tau apply_s2_tau1;
        let app_typ = T.TyFunApp { t1 = tau2; t2 = TyVar new_tyvar } in 
        print_string ("Unify: " ^ PR.string_of_tau apply_s2_tau1 ^ " " ^ PR.string_of_tau app_typ ^ "\n");
        let s3 =
          unify apply_s2_tau1 app_typ
        in
        print_string "Unification substitution: ";
        PR.print_substitution s3;
        let apply_s3_to_new_tyvar= S.apply s3 (TyVar new_tyvar) in
        print_string "S3(new_tyvar): ";
        PR.print_tau apply_s3_to_new_tyvar;
        print_newline(); 
        (S.compose s3 (S.compose s2 s1), apply_s3_to_new_tyvar)
    | A.Let { id; e1; e2 } ->
        print_string "Let case \n";
        print_string "Traversing e1 \n \n";
        let s1, tau1 = trav gamma e1 in
        print_string "Let e1's substitution: ";
        PR.print_substitution s1;
        print_string "Let e1's tau: ";
        PR.print_tau tau1;
        let s1_gamma = S.apply_to_gamma s1 gamma in
        print_string "S1 \\gamma";
        PR.print_gamma s1_gamma;
        print_string ("Calling clos with and above: " ^ PR.string_of_tau tau1^ "\n");
        let clos_s1_gamma_tau = clos s1_gamma (TE.wrap_monotype tau1) in
        print_string "Clos typescheme: ";
        PR.print_typescheme clos_s1_gamma_tau;
        let gamma_ext = TE.add id clos_s1_gamma_tau gamma in
        print_string "Adding above to gamma - this is \\gamma_ext:";
        PR.print_gamma gamma_ext;
        let s1_gamma_ext = S.apply_to_gamma s1 gamma_ext in
        print_string "S1 \\gamma_ext";
        PR.print_gamma s1_gamma_ext;
        print_string "Traversing e2 \n \n";
        let s2, tau2 = trav s1_gamma_ext e2 in
        print_string "Let e2's substitution: ";
        PR.print_substitution s2;
        print_string "Let e2's tau: ";
        PR.print_tau tau2;
        print_newline(); 
        (S.compose s2 s1, tau2)
    | A.Tuple { e1; e2 } ->
        print_string "Tuple case \n";
        print_string "Traversing e1 \n \n";
        let s1, tau1 = trav gamma e1 in
        print_string "Tuple e1's substitution: ";
        PR.print_substitution s1;
        print_string "Tuple e1's tau: ";
        PR.print_tau tau1;
        print_string "Traversing e2 \n \n";
        let s2, tau2 = trav (S.apply_to_gamma s1 gamma) e2 in
        print_string "Tuple e2's substitution: ";
        PR.print_substitution s2;
        print_string "Tuple e2's tau: ";
        PR.print_tau tau2;
        print_newline(); 
        (S.compose s2 s1, S.apply s2 (TyTuple { t1 = tau1; t2 = tau2 }))
    | A.Fst e1 -> (
        print_string "Fst case \n";
        print_string "Traversing e1 \n \n";
        let s1, tau1 = trav gamma e1 in
        PR.print_substitution s1;
        PR.print_tau tau1;
        print_newline(); 
        match tau1 with TyTuple { t1; _ } -> (s1, t1) | _ -> raise Fail)
    | A.Snd e1 -> (
        print_string "Snd case \n";
        print_string "Traversing e1 \n \n";
        let s1, tau1 = trav gamma e1 in
        PR.print_substitution s1;
        PR.print_tau tau1;
        print_newline(); 
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
  (* fun x -> let y = (fun w -> w) x in fun u -> fun z -> (y u, y z) *)
  run_example EX.everything_example "Everything_example \n";
  (* fun x -> let y = fun w -> (w x) in fun u -> fun z -> (y u, y z) *)
  run_example EX.everything_example2 "Everything_example2 \n";
  (*
     let id = fun x -> x in
     let both = (id 2, id true) in
     both
  *)
  run_example EX.polymorphic_id_with_int_and_bool
    "Polymorphic_id_with_int_and_bool \n";
  run_example EX.debug_example "debug \n"
