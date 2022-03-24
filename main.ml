open Src
open Types 

module A = Ast
module TE = TypeEnv
module PR = PrettyPrinter
module EX = Examples

exception Fail
exception Impossible
exception FailMessage of string

let counter = ref 0
let get_next_tyvar () =
  counter := !counter + 1;
  !counter

let new_node () = UF.make_set (TyVar (get_next_tyvar()))
let ( ~% ) alpha = UF.root alpha
let ( ~& ) gamma = TE.update_typeschemes gamma
let ( +- ) gamma (id, ts) = TE.add id ts gamma
let ( ~$ ) t = TE.wrap_monotype t
let ( => ) t1 t2 = UF.make_set (TyFunApp { t1; t2 })
let ( ** ) t1 t2 = UF.make_set (TyTuple { t1; t2 })

let list_of_set set = List.of_seq (SS.to_seq set)
let combine_sets sets = List.fold_left (fun a b -> SS.union a b) SS.empty sets

let rec find_tyvars (tau_node : typ_node) : SS.t = match UF.find tau_node with
  | TyCon _ -> SS.empty
  | TyVar alpha -> SS.singleton alpha
  | TyFunApp { t1; t2 } -> SS.union (find_tyvars t1) (find_tyvars t2)
  | TyTuple { t1; t2 } -> SS.union (find_tyvars t1) (find_tyvars t2)

let find_free_tyvars (TypeScheme {tyvars; tau_node}) =
  SS.diff (find_tyvars tau_node) tyvars

let clos (gamma : typescheme TE.Gamma.t) tau_node =
  let free_tyvars_tau = find_free_tyvars (~$tau_node) in
  let free_tyvars_gamma = combine_sets (List.map (fun (_, v) -> find_free_tyvars v) (TE.bindings gamma)) in 
  TypeScheme { tyvars = SS.diff free_tyvars_tau free_tyvars_gamma; tau_node }

let occurs_check tyvar (tau: typescheme) = 
  if SS.mem tyvar (find_free_tyvars tau) then raise (FailMessage "recursive unification")

let rec unify (t1: typ_node) (t2: typ_node): unit =
  let t1, t2 = ~%t1, ~%t2 in
  match UF.find t1, UF.find t2 with
  | TyCon c1, TyCon c2 -> if c1 = c2 then () else raise (FailMessage "cannot unify")
  | TyVar ty_var1, TyVar ty_var2 -> if ty_var1 = ty_var2 then () else UF.union t1 t2
  | TyVar ty_var, _ -> occurs_check ty_var ~$t2; UF.union t1 t2
  | _, TyVar ty_var -> occurs_check ty_var ~$t1; UF.union t1 t2 (* maybe other way around? *)
  | TyFunApp { t1 = t11; t2 = t12 }, TyFunApp { t1 = t21; t2 = t22 }
  | TyTuple { t1 = t11; t2 = t12 }, TyTuple { t1 = t21; t2 = t22 } ->
      unify t11 t21;
      unify ~%t12 ~%t22
  | _ -> raise Fail

let specialize (TypeScheme{tyvars; tau_node}) =  
  let bindings = List.map (fun tv -> (tv, get_next_tyvar())) (list_of_set tyvars) in
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
  let rec w (gamma : TE.map_type) exp : typ_node =
    match exp with
    | A.Var id -> (
        match TE.look_up id gamma with
        | None -> raise Fail
        | Some ts -> specialize ts)
    | A.Lambda { id; e1 } ->
        let alpha = new_node () in
        let tau1_node = w (gamma +- (id, ~$alpha)) e1 in
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
        match UF.find tau1_node with TyTuple { t1; _ } -> t1 | _ -> raise Fail)
    | A.Snd e1 -> (
        let tau1_node = w gamma e1 in
        match UF.find tau1_node with TyTuple { t2; _ } -> t2 | _ -> raise Fail)
    | A.BasVal b ->
        match b with
        | Int _ -> UF.make_set (TyCon Int)
        | Bool _ -> UF.make_set (TyCon Bool)
        | String _ -> UF.make_set (TyCon String)
  in
  w TE.empty exp

let run_example ast name =
  print_newline ();
  print_string name;
  PR.print_tau_node (infer_type ast);
  counter := 0

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