open Types
open Utils

module A = Ast

let rec find_tyvars (tau : typ) : SS.t = match ~%tau with
  | TyCon _ -> SS.empty
  | TyVar {contents = Link t} -> find_tyvars t
  | TyVar {contents = Int alpha} -> SS.singleton alpha
  | TyFunApp { t1; t2 } -> SS.union (find_tyvars t1) (find_tyvars t2)
  | TyTuple { t1; t2 } -> SS.union (find_tyvars t1) (find_tyvars t2)

let find_free_tyvars (TypeScheme {tyvars; tau}) =
  SS.diff (find_tyvars tau) tyvars

let clos (gamma : typescheme TE.Gamma.t) tau =
  let free_tyvars_tau = find_free_tyvars (!$tau) in
  let free_tyvars_gamma = combine_sets (List.map (fun (_, v) -> find_free_tyvars v) (TE.bindings gamma)) in 
  TypeScheme { tyvars = SS.diff free_tyvars_tau free_tyvars_gamma; tau }

let occurs_check tyvar (tau: typescheme) =
  if SS.mem tyvar (find_free_tyvars tau) then raise (Fail "recursive unification")

let rec unify (t1: typ) (t2: typ): unit =
  let t1, t2 = ~%t1, ~%t2 in
  match t1, t2 with
  | TyCon c1, TyCon c2 -> if c1 = c2 then () else raise (Fail "cannot unify")
  | TyVar tv1, TyVar tv2 -> if tv1 = tv2 then () else union tv1 t2
  | TyVar ({contents = Int i} as tv), _ -> occurs_check i !$t2; union tv t2
  | _, TyVar ({contents = Int i} as tv) -> occurs_check i !$t1; union tv t2
  | TyFunApp { t1 = t11; t2 = t12 }, TyFunApp { t1 = t21; t2 = t22 }
  | TyTuple { t1 = t11; t2 = t12 }, TyTuple { t1 = t21; t2 = t22 } ->
      unify t11 t21;
      unify t12 t22
  | _ -> raise (Fail "unify _ case")

let specialize (TypeScheme{tyvars; tau}) =
  let bindings = List.map (fun tv -> (tv, TE.get_next_tyvar())) (list_of_set tyvars) in
  let rec subst_tyvars (tau: typ) =
    let tau = ~%tau in
    match tau with
    | TyCon _ -> tau
    | TyVar {contents = Int i} -> (
      match List.assoc_opt i bindings with
      | Some res -> TyVar (ref (Int res))
      | None -> tau)
    | TyVar _ -> raise (Fail "specialize")
    | TyFunApp {t1; t2} -> subst_tyvars t1 => subst_tyvars t2
    | TyTuple {t1; t2} -> subst_tyvars t1 ** subst_tyvars t2
  in
  subst_tyvars tau

let infer_type (exp : A.exp) : typ =
  let rec w (gamma : TE.ts_map) exp : typ =
    match exp with
    | A.Var id -> (
        print_string "id\n";
        match TE.look_up id gamma with
        | None -> raise (Fail "id not in type environment")
        | Some ts -> specialize ts)
    | A.Lambda { id; e1 } ->
        print_string "lambda\n";
        let alpha = new_tyvar () in
        let tau1 = w (gamma +- (id, !$alpha)) e1 in
        ~%alpha => tau1
    | A.App { e1; e2 } ->
        print_string "app\n";
        let tau1 = w gamma e1 in
        let tau2 = w ~&gamma e2 in
        let alpha = new_tyvar () in
        unify ~%tau1 (tau2 => alpha);
        ~%alpha
    | A.Let { id; e1; e2 } ->
        print_string "let\n";
        let tau1 = w gamma e1 in
        let s1_gamma = ~&gamma in
        let tau2 = w (s1_gamma +- (id, clos s1_gamma tau1)) e2 in
        tau2
    | A.Tuple { e1; e2 } ->
        print_string "tuple\n";
        let tau1 = w gamma e1 in
        let tau2 = w ~&gamma e2 in
        tau1 ** tau2
    | A.Fst e1 -> (
        print_string "fst\n";
        let tau1 = w gamma e1 in
        match ~%tau1 with TyTuple { t1; _ } -> t1 | _ -> raise (Fail "expected tuple"))
    | A.Snd e1 -> (
        print_string "snd\n";
        let tau1 = w gamma e1 in
        match ~%tau1 with TyTuple { t2; _ } -> t2 | _ -> raise (Fail "expected tuple"))
    | A.BasVal b ->
        print_string "basval\n";
        match b with
        | Int _ -> TyCon Int
        | Bool _ -> TyCon Bool
        | String _ -> TyCon String
  in
  w TE.empty exp