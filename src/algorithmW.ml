open Types
open Utils

module A = Ast

exception Fail of string

let rec find_tyvars (tau_node : typ_node) : SS.t = match UF.find tau_node with
  | TyCon _ -> SS.empty
  | TyVar alpha -> SS.singleton alpha
  | TyFunApp { t1; t2 } -> SS.union (find_tyvars t1) (find_tyvars t2)
  | TyTuple { t1; t2 } -> SS.union (find_tyvars t1) (find_tyvars t2)

let find_free_tyvars (TypeScheme {tyvars; tau_node}) =
  SS.diff (find_tyvars tau_node) tyvars

let clos (gamma : typescheme TE.Gamma.t) tau_node =
  let free_tyvars_tau = find_free_tyvars (!$tau_node) in
  let free_tyvars_gamma = combine_sets (List.map (fun (_, v) -> find_free_tyvars v) (TE.bindings gamma)) in 
  TypeScheme { tyvars = SS.diff free_tyvars_tau free_tyvars_gamma; tau_node }

let occurs_check tyvar (tau: typescheme) = 
  if SS.mem tyvar (find_free_tyvars tau) then raise (Fail "recursive unification")

let rec unify (t1: typ_node) (t2: typ_node): unit =
  let t1, t2 = ~%t1, ~%t2 in
  match UF.find t1, UF.find t2 with
  | TyCon c1, TyCon c2 -> if c1 = c2 then () else raise (Fail "cannot unify")
  | TyVar ty_var1, TyVar ty_var2 -> if ty_var1 = ty_var2 then () else UF.union t1 t2
  | TyVar ty_var, _ -> occurs_check ty_var !$t2; UF.union t1 t2
  | _, TyVar ty_var -> occurs_check ty_var !$t1; UF.union t1 t2 (* maybe other way around? *)
  | TyFunApp { t1 = t11; t2 = t12 }, TyFunApp { t1 = t21; t2 = t22 }
  | TyTuple { t1 = t11; t2 = t12 }, TyTuple { t1 = t21; t2 = t22 } ->
      unify t11 t21;
      unify ~%t12 ~%t22
  | _ -> raise (Fail "unify _ case")

let specialize (TypeScheme{tyvars; tau_node}) =  
  let bindings = List.map (fun tv -> (tv, TE.get_next_tyvar())) (list_of_set tyvars) in
  let rec subst_tyvars (tau_node: typ_node) =
    let tau_node = ~%tau_node in
    match UF.find tau_node with
    | TyCon _ -> tau_node
    | TyVar tyvar -> (
      match List.assoc_opt tyvar bindings with
      | Some res -> UF.make_set (TyVar res)
      | None -> tau_node)
    | TyFunApp {t1; t2} -> UF.make_set (TyFunApp {t1 = subst_tyvars t1; t2 = subst_tyvars t2})
    | TyTuple {t1; t2} -> UF.make_set (TyTuple {t1 = subst_tyvars t1; t2 = subst_tyvars t2})
  in
  subst_tyvars tau_node

let infer_type (exp : A.exp) : typ_node =
  let rec w (gamma : TE.ts_map) exp : typ_node =
    match exp with
    | A.Var id -> (
        match TE.look_up id gamma with
        | None -> raise (Fail "id not in type environment")
        | Some ts -> specialize ts)
    | A.Lambda { id; e1 } ->
        let alpha = new_node () in
        let tau1_node = w (gamma +- (id, !$alpha)) e1 in
        ~%alpha => tau1_node
    | A.App { e1; e2 } ->
        let tau1_node = w gamma e1 in
        let tau2_node = w ~&gamma e2 in
        let alpha = new_node () in
        unify ~%tau1_node (tau2_node => alpha);
        ~%alpha
    | A.Let { id; e1; e2 } ->
        let tau1_node = w gamma e1 in
        let s1_gamma = ~&gamma in
        let tau2_node = w (s1_gamma +- (id, clos s1_gamma tau1_node)) e2 in
        tau2_node
    | A.Tuple { e1; e2 } ->
        let tau1_node = w gamma e1 in
        let tau2_node = w ~&gamma e2 in
        tau1_node ** tau2_node
    | A.Fst e1 -> (
        let tau1_node = w gamma e1 in
        match UF.find tau1_node with TyTuple { t1; _ } -> t1 | _ -> raise (Fail "expected tuple"))
    | A.Snd e1 -> (
        let tau1_node = w gamma e1 in
        match UF.find tau1_node with TyTuple { t2; _ } -> t2 | _ -> raise (Fail "expected tuple"))
    | A.BasVal b ->
        UF.make_set (
          match b with
          | Int _ -> TyCon Int
          | Bool _ -> TyCon Bool
          | String _ -> TyCon String)
  in
  w TE.empty exp