open Types
open Utils

module A = Ast

let rec find_tyvars tau = match ~$tau with
  | TyCon _ -> SS.empty
  | TyVar {contents = Int alpha} -> SS.singleton alpha
  | TyVar _ -> raise (Fail "find_tyvars link")
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
  | TyCon c1, TyCon c2 -> if c1 = c2 then () else raise (Fail "cannot unify")
  | TyVar tv1, TyVar tv2 -> if tv1 = tv2 then () else union tv1 t2
  | TyVar ({contents = Int i} as tv), _ -> occurs_check i !&t2; union tv t2
  | _, TyVar ({contents = Int i} as tv) -> occurs_check i !&t1; union tv t1
  | TyFunApp { t1 = t11; t2 = t12 }, TyFunApp { t1 = t21; t2 = t22 }
  | TyTuple { t1 = t11; t2 = t12 }, TyTuple { t1 = t21; t2 = t22 } ->
      unify ~$t11 ~$t21;
      unify ~$t12 ~$t22
  | _ -> raise (Fail "unify _ case")

let specialize (TypeScheme{tyvars; tau}) =
  let bindings = List.map (fun tv -> (tv, new_tyvar())) (SS.elements tyvars) in
  let rec subst_tyvars tau =
    let tau = ~$tau in
    match tau with
    | TyCon _ -> tau
    | TyVar {contents = Int i} -> assoc_or_else bindings i ~default:tau
    | TyVar _ -> raise (Fail "specialize link")
    | TyFunApp {t1; t2} -> subst_tyvars t1 => subst_tyvars t2
    | TyTuple {t1; t2} -> subst_tyvars t1 ** subst_tyvars t2
  in
  subst_tyvars tau

let infer_type exp =
  let rec w gamma exp =
    match exp with
    | A.Var id -> (
        match TE.look_up id gamma with
        | None -> raise (Fail "id not in type environment")
        | Some ts -> specialize ts)
    | A.Lambda { id; e1 } ->
        let alpha = new_tyvar () in
        let tau1 = w (gamma +- (id, !&alpha)) e1 in
        ~$alpha => tau1
    | A.App { e1; e2 } ->
        let tau1 = w gamma e1 in
        let tau2 = w gamma e2 in
        let alpha = new_tyvar () in
        unify ~$tau1 (tau2 => alpha);
        ~$alpha
    | A.Let { id; e1; e2 } ->
        let tau1 = w gamma e1 in
        let s1_gamma = gamma in
        let tau2 = w (s1_gamma +- (id, clos s1_gamma tau1)) e2 in
        tau2
    | A.Tuple { e1; e2 } ->
        let tau1 = w gamma e1 in
        let tau2 = w gamma e2 in
        tau1 ** tau2
    | A.Fst e1 -> (
        let tau1 = w gamma e1 in
        match ~$tau1 with TyTuple { t1; _ } -> t1 | _ -> raise (Fail "expected tuple"))
    | A.Snd e1 -> (
        let tau1 = w gamma e1 in
        match ~$tau1 with TyTuple { t2; _ } -> t2 | _ -> raise (Fail "expected tuple"))
    | A.BasVal b ->
        match b with
        | Int _ -> TyCon Int
        | Bool _ -> TyCon Bool
        | String _ -> TyCon String
  in
  w TE.empty exp