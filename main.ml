open Src

module TE = TypeEnv
module PR = PrettyPrinter
module EX = Examples
module AW = AlgorithmW

let run_example ast name =
  print_newline ();
  print_string name;
  PR.print_tau_node (AW.infer_type ast);
  TE.reset ()

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