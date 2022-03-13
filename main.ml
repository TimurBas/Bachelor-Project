open Src
module A = Ast
module T = Types
module TE = TypeEnv
module S = Substitution
module PR = PrettyPrinter
module EX = Examples
module SS = Set.Make(Int)

exception Fail
exception Impossible
exception FailMessage of string

let rec find_tyvars (tau : T.typ) : SS.t = match tau with
  | TyCon _ -> SS.empty
  | TyVar alpha -> SS.singleton alpha
  | TyFunApp { t1; t2 } -> SS.union (find_tyvars t1) (find_tyvars t2)
  | TyTuple { t1; t2 } -> SS.union (find_tyvars t1) (find_tyvars t2)

let find_free_tyvars (T.TypeScheme { tyvars; tau }) =
  SS.diff (find_tyvars tau) tyvars

let clos (gamma : T.typescheme TE.Gamma.t) tau =
  let free_tyvars_tau = find_free_tyvars (TE.wrap_monotype tau) in
  let gamma_typeschemes = List.map (fun (_, v) -> v) (TE.bindings gamma) in
  let free_tyvars_gamma = List.fold_left (fun acc elt -> SS.union acc (find_free_tyvars elt)) SS.empty gamma_typeschemes in
  (* let free_tyvars_gamma = SS.of_list (List.concat_map (fun s -> SS.elements s) gamma_typeschemes) in *)
  let diff = SS.diff free_tyvars_tau free_tyvars_gamma in 
  T.TypeScheme { tyvars = diff; tau }

let occurs_check tyvar tau = 
  let free_tyvars = find_free_tyvars (TE.wrap_monotype tau) in 
  if SS.mem tyvar free_tyvars then raise (FailMessage "Recursive unification")

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
            let subst_bindings = List.map (fun tv -> (tv, T.TyVar (get_next_tyvar()))) (List.of_seq (SS.to_seq tyvars)) in
            let subst = S.of_list subst_bindings in
            (S.empty, S.apply subst tau)
        | None -> raise Fail)
    | A.Lambda { id; e1 } ->
        let new_tyvar = get_next_tyvar () in
        let gamma_ext = TE.add_alpha id (TyVar new_tyvar) gamma in 
        let s, tau = trav gamma_ext e1 in
        let tau' =
          match S.look_up new_tyvar s with
          | Some typ -> typ
          | None -> T.TyVar new_tyvar
        in
        (s, TyFunApp { t1 = tau'; t2 = tau })
    | A.App { e1; e2 } ->
        let s1, tau1 = trav gamma e1 in
        let s1_applied_to_gamma = (S.apply_to_gamma s1 gamma) in 
        let s2, tau2 = trav s1_applied_to_gamma e2 in
        let new_tyvar = get_next_tyvar () in
        let apply_s2_tau1 = S.apply s2 tau1 in 
        let app_typ = T.TyFunApp { t1 = tau2; t2 = TyVar new_tyvar } in 
        let s3 =
          unify apply_s2_tau1 app_typ
        in
        let apply_s3_to_new_tyvar= S.apply s3 (TyVar new_tyvar) in
        (S.compose s3 (S.compose s2 s1), apply_s3_to_new_tyvar)
    | A.Let { id; e1; e2 } ->
        let s1, tau1 = trav gamma e1 in
        let s1_gamma = S.apply_to_gamma s1 gamma in
        let clos_s1_gamma_tau1 = clos s1_gamma tau1 in
        let gamma_ext = TE.add id clos_s1_gamma_tau1 s1_gamma in
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
        | Int _ -> 
          (empty_map, TyCon Int)
        | Bool _ -> 
          (empty_map, TyCon Bool)
        | String _ -> 
          (empty_map, TyCon String))
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
  (* fun x -> fun y -> x y *)
  run_example EX.fun_application_example "Fun_application_example \n";
  (* fun x -> fun y -> fun z -> z (x y) *)
  run_example EX.fun_application_three_example
    "Fun_application_three_example \n";
  (* fun x -> fun y -> (x y, x y) *)
  run_example EX.tuple_fun_application_example
    "Tuple_fun_application_example \n";
  (* fun x -> fun z -> let y = fun w -> (w x, w x) in y *)
  run_example EX.lambda_outside_let_example "Lambda_outside_let_example \n";
  (* fun x -> fst (x, x) *)
  run_example EX.fst_lambda_example "Fst_lambda_example \n";
  (* fun x -> fun y -> let z = (y, x) in fst z *)
  run_example EX.fst_let_example "Fst_let_example \n";
  (* fun x -> let y = (fun w -> w) x in fun u -> fun z -> (y u, y z) *)
  run_example EX.everything_example "Everything_example \n";
  (* fun x -> let y = fun w -> (w x) in fun u -> fun z -> (y u, y z) *)
  run_example EX.everything_example2 "Everything_example2 \n";
  (* let id = fun x -> x in let both = (id 2, id true) in both *)
  run_example EX.polymorphic_id_with_int_and_bool
    "Polymorphic_id_with_int_and_bool \n";
  (* 
    let id = fun x -> x in 
    let pair = fun p -> fun x -> fun y -> (p x) y in  
    let p1 = fun p -> ((pair id) id) p in 
    let p2 = fun p -> pair p1 p1 p in 
    let p3 = fun p -> pair p2 p2 p in 
    let p4 = fun p -> pair p3 p3 p in 
    let p5 = fun p -> pair p4 p4 p in 
    ((((((p5, p4), p3), p2), p1), pair), id) *)
  run_example EX.many_nested_lets "Many_nested_lets \n";
  run_example EX.many_lambdas "Many lambdas \n";
  run_example EX.debug_example "debug \n"
