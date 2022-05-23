open Types
open Utils

module A = Ast
module S = Substitution

let rec find_tyvars tau = match tau with
  | TyCon _ -> SS.empty
  | TyVar alpha -> SS.singleton alpha
  | TyFunApp { t1; t2 } -> SS.union (find_tyvars t1) (find_tyvars t2)
  | TyTuple { t1; t2 } -> SS.union (find_tyvars t1) (find_tyvars t2)

let find_free_tyvars (TypeScheme {tyvars; tau}) =
  SS.diff (find_tyvars tau) tyvars

let clos gamma tau =
  let free_tyvars_tau = find_free_tyvars (!&tau) in
  let free_tyvars_gamma = combine_sets (List.map (fun (_, v) -> find_free_tyvars v) (TE.bindings gamma)) in 
  TypeScheme { tyvars = SS.diff free_tyvars_tau free_tyvars_gamma; tau }

let occurs_check tyvar tau =
  if SS.mem tyvar (find_free_tyvars tau) then raise (Fail "recursive unification")

let rec unify t1 t2 =
  match t1, t2 with
  | TyCon c1, TyCon c2 -> if c1 = c2 then S.empty else raise (Fail "cannot unify")
  | TyVar tv1, TyVar tv2 -> if tv1 = tv2 then S.empty else S.add tv1 (TyVar tv2) S.empty
  | TyVar tv, _ -> occurs_check tv !&t2; S.add tv t2 S.empty
  | _, TyVar tv -> occurs_check tv !&t1; S.add tv t1 S.empty
  | TyFunApp { t1 = t11; t2 = t12 }, TyFunApp { t1 = t21; t2 = t22 }
  | TyTuple { t1 = t11; t2 = t12 }, TyTuple { t1 = t21; t2 = t22 } ->
    let s1 = unify t11 t21 in
    let s2 = unify (S.apply s1 t12) (S.apply s1 t22) in
    S.compose s2 s1
  | _ -> raise (Fail "unify _ case")

let specialize (TypeScheme{tyvars; tau}) =
  let bindings = List.map (fun tv -> (tv, new_tyvar())) (SS.elements tyvars) in
  let subst = S.of_list bindings in
  (S.empty, S.apply subst tau)

let infer_type exp =
  let rec w gamma exp =
    match exp with
    | A.Var id -> (
      match TE.look_up id gamma with
      | None -> raise (Fail "id not in type environment")
      | Some ts -> specialize ts)
    | A.Lambda { id; e1 } ->
      let alpha = new_tyvar () in
      let s1, tau1 = w (gamma +- (id, !&alpha)) e1 in
      (s1, S.apply s1 alpha => tau1)
    | A.App { e1; e2 } ->
      let (s1, tau1) = w gamma e1 in
      let (s2, tau2) = w (S.apply_to_gamma s1 gamma) e2 in
      let alpha = new_tyvar () in
      let s3 = unify (S.apply s2 tau1) (tau2 => alpha) in
      (S.compose s3 (S.compose s2 s1), S.apply s3 alpha)
    | A.Let { id; e1; e2 } ->
      let (s1, tau1) = w gamma e1 in
      let s1_gamma = S.apply_to_gamma s1 gamma in
      let (s2, tau2) = w (s1_gamma +- (id, clos s1_gamma tau1)) e2 in
      (S.compose s2 s1, tau2)
    | A.Tuple { e1; e2 } ->
      let (s1, tau1) = w gamma e1 in
      let (s2, tau2) = w (S.apply_to_gamma s1 gamma) e2 in
      (S.compose s2 s1, (S.apply s2 tau1) ** tau2)
    | A.Fst e1 -> (
      let (s1, tau1) = w gamma e1 in
      match tau1 with TyTuple { t1; _ } -> (s1, t1) | _ -> raise (Fail "expected tuple"))
    | A.Snd e1 -> (
      let (s1, tau1) = w gamma e1 in
      match tau1 with TyTuple { t2; _ } -> (s1, t2) | _ -> raise (Fail "expected tuple"))
    | A.BasVal b ->
      match b with
      | Int _ -> (S.empty, TyCon Int)
      | Bool _ -> (S.empty, TyCon Bool)
      | String _ -> (S.empty, TyCon String)
  in
  snd (w TE.empty exp)