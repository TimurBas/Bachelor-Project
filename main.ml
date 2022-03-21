open Src
open Types 

module A = Ast
module TE = TypeEnv
module PR = PrettyPrinter
module EX = Examples
module UF = UnionFind
module SS = Set.Make(Int)

exception Fail
exception Impossible
exception FailMessage of string

let list_of_set set = List.of_seq (SS.to_seq set)

let rec find_tyvars (tau : typ UF.node) : SS.t = match tau with
  | TyCon _ -> SS.empty
  | TyVar alpha -> SS.singleton alpha
  | TyFunApp { t1; t2 } -> SS.union (find_tyvars t1) (find_tyvars t2)
  | TyTuple { t1; t2 } -> SS.union (find_tyvars t1) (find_tyvars t2)

let find_free_tyvars (TypeScheme {tyvars; tau}) =
  SS.diff (find_tyvars tau) tyvars

let clos (gamma : typescheme TE.Gamma.t) tau =
  let free_tyvars_tau = find_free_tyvars (TE.wrap_monotype tau) in
  let gamma_typeschemes = List.map (fun (_, v) -> v) (TE.bindings gamma) in
  let free_tyvars_gamma = List.fold_left (fun acc elt -> SS.union acc (find_free_tyvars elt)) SS.empty gamma_typeschemes in
  (* let free_tyvars_gamma = SS.of_list (List.concat_map (fun s -> SS.elements s) gamma_typeschemes) in *)
  let diff = SS.diff free_tyvars_tau free_tyvars_gamma in 
  TypeScheme { tyvars = diff; tau }

let occurs_check tyvar (tau: typescheme) = 
  let free_tyvars = find_free_tyvars tau in 
  if SS.mem tyvar free_tyvars then raise (FailMessage "Recursive unification")

let counter = ref 0

let get_next_tyvar () =
  counter := !counter + 1;
  !counter

let rec unify (t1: typ) (t2: typ): unit =
  match t1, t2 with 
  | TyVar ty_var1, TyVar ty_var2 when ty_var1 = ty_var2 -> ()
  | TyVar n1, TyVar n2 -> UF.union n1 n2
  | TyVar ty_var, _ -> occurs_check ty_var (TE.wrap_monotype n2); UF.union n1 n2
  | _, TyVar ty_var -> occurs_check ty_var (TE.wrap_monotype n1); UF.union n1 n2
  | TyFunApp { t1 = t11; t2 = t12 }, TyFunApp { t1 = t21; t2 = t22 } 
  | TyTuple { t1 = t11; t2 = t12 }, TyTuple { t1 = t21; t2 = t22 } ->
    unify t11 t21;
    unify (UF.find t12) (UF.find t22)
  | TyCon c1, TyCon c2 when c1 = c2 -> ()
  | _ -> raise Fail

let specialize (TypeScheme{tyvars; tau}) =  
  let bindings = List.map (fun tv -> (tv, get_next_tyvar())) (list_of_set tyvars) in
  let rec subst_tyvars (tau: typ) =
    match tau with
    | TyCon _ -> tau
    | TyVar tyvar -> (
      match List.assoc_opt tyvar bindings with
      | Some res -> UF.make_set (TyVar res)
      | None -> tau)
    | TyFunApp {t1; t2} -> UF.make_set (TyFunApp {t1 = subst_tyvars t1; t2 = subst_tyvars t2})
    | TyTuple {t1; t2} -> UF.make_set (TyTuple {t1 = subst_tyvars t1; t2 = subst_tyvars t2})
  in
  subst_tyvars tau

let algorithm_w (exp : A.exp) : typ =
  let rec trav (gamma : TE.map_type) exp : typ =
    match exp with
    | A.Var id -> (
        match TE.look_up id gamma with
        | None -> raise Fail
        | Some ts -> specialize ts)
    | A.Lambda { id; e1 } ->
        let new_node = UF.make_set (TyVar (get_next_tyvar())) in
        let gamma_ext = TE.add_alpha id (UF.find(!(new_node))) gamma in 
        let tau1_node = trav gamma_ext e1 in
        TyFunApp {t1 = UF.find !new_node; t2 = tau1_node}
    | A.App { e1; e2 } ->
        let tau1_node = trav gamma e1 in
        let s1_applied_to_gamma = TE.update_typeschemes gamma in
        let tau2_node = trav s1_applied_to_gamma e2 in (* infix operator for apply *)
        let new_tyvar_node = UF.make_set (get_next_tyvar()) in
        let app_typ = TyFunApp { t1 = tau2_node; t2 = UF.find(!(new_tyvar_node)) } in (* maybe infix operator for TyFunApp like an arrow *)
        unify tau1_node app_typ;
        UF.find !new_tyvar_node (* infix operator for compose *)
    | A.Let { id; e1; e2 } ->
        let tau1_node = trav gamma e1 in
        let s1_gamma = TE.update_typeschemes gamma in
        let clos_s1_gamma_tau1 = clos s1_gamma tau1_node in
        let gamma_ext = TE.add id clos_s1_gamma_tau1 s1_gamma in
        let s1_gamma_ext = TE.update_typeschemes gamma_ext in
        let tau2_node = trav s1_gamma_ext e2 in
        tau2_node
    | A.Tuple { e1; e2 } ->
        let tau1_node = trav gamma e1 in
        let tau2_node = trav (TE.update_typeschemes gamma) e2 in
        TyTuple { t1 = tau1_node; t2 = tau2_node }
    | A.Fst e1 -> (
        let tau1_node = trav gamma e1 in
        match tau1_node with TyTuple { t1; _ } -> t1 | _ -> raise Fail)
    | A.Snd e1 -> (
        let tau1_node = trav gamma e1 in
        match tau1_node with TyTuple { t2; _ } -> t2 | _ -> raise Fail)
    | A.BasVal b ->
        match b with
        | Int _ -> TyCon Int
        | Bool _ -> TyCon Bool
        | String _ -> TyCon String
  in
  trav TE.empty exp

let run_example ast name =
  print_newline ();
  print_string name;
  let tau = algorithm_w ast in
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
