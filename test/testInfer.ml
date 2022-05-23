open! Import

open PrettyPrinter
open AlgorithmW
open Examples

let%expect_test "Non_polymorphic_id_example" =
  print_tau (infer_type non_polymorphic_id_example);
  TypeEnv.reset ();
  [%expect {| 1 -> 2 -> 3 -> 3|}]

let%expect_test "Polymorphic_id_example" =
  print_tau (infer_type polymorphic_id_example);
  TypeEnv.reset ();
  [%expect {| 2 -> 2 |}]

let%expect_test "Nested_let_example" =
  print_tau (infer_type nested_let_example);
  TypeEnv.reset ();
  [%expect {| (9 -> 10 -> 11 -> 12 -> 12) x (13 -> 14 -> 15 -> 16 -> 16) |}]

let%expect_test "Fun_application_example" =
  print_tau (infer_type fun_application_example);
  TypeEnv.reset ();
  [%expect {| (2 -> 3) -> 2 -> 3 |}]

let%expect_test "Fun_application_three_example" =
  print_tau (infer_type fun_application_three_example);
  TypeEnv.reset ();
  [%expect {| (2 -> 4) -> 2 -> (4 -> 5) -> 5 |}]

  let%expect_test "Tuple_fun_application_example" =
  print_tau (infer_type tuple_fun_application_example);
  TypeEnv.reset ();
  [%expect {| (2 -> 4) -> 2 -> 4 x 4 |}]

let%expect_test "Lambda_outside_let_example" =
  print_tau (infer_type lambda_outside_let_example);
  TypeEnv.reset ();
  [%expect {| 1 -> 2 -> (1 -> 6) -> 6 x 6 |}]

let%expect_test "Fst_lambda_example" =
  print_tau (infer_type fst_lambda_example);
  TypeEnv.reset ();
  [%expect {| 1 -> 1 |}]

let%expect_test "Fst_let_example " =
  print_tau (infer_type fst_let_example);
  TypeEnv.reset ();
  [%expect {| 1 -> 2 -> 2 |}]

let%expect_test "Everything_example" =
  print_tau (infer_type everything_example);
  TypeEnv.reset ();
  [%expect {| (5 -> 7) -> 5 -> 5 -> 7 x 7 |}]

let%expect_test "Everything_example" =
  print_tau (infer_type everything_example2);
  TypeEnv.reset ();
  [%expect {| 1 -> (1 -> 7) -> (1 -> 9) -> 7 x 9 |}]

let%expect_test "Polymorphic_id_with_int_and_bool" =
  print_tau (infer_type polymorphic_id_with_int_and_bool);
  TypeEnv.reset ();
  [%expect {| int x bool |}]

let%expect_test "Many_nested_let" =
  print_tau (infer_type many_nested_lets);
  TypeEnv.reset ();
  [%expect {| ((((((52 -> 52) x (53 -> 53)) x (54 -> 54)) x (55 -> 55)) x (56 -> 56)) x ((57 -> 58 -> 59) -> 57 -> 58 -> 59)) x (60 -> 60) |}]

let%expect_test "Many_lambdas" =
  print_tau (infer_type many_lambdas);
  TypeEnv.reset ();
  [%expect {| 1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> 8 -> 9 -> 10 -> 11 -> 12 -> 13 -> 14 -> 15 -> 16 -> 17 -> 18 -> 19 -> 20 -> 21 -> 22 -> 23 -> 24 -> 25 -> 26 -> 27 -> 28 -> 29 -> 30 -> 31 -> 32 -> 33 -> 34 -> 35 -> 36 -> 37 -> 38 -> 39 -> 40 -> 41 -> 42 -> 43 -> 44 -> 45 -> 46 -> 47 -> 48 -> 49 -> 50 -> 51 -> 52 -> 53 -> 54 -> 55 -> 56 -> 57 -> 58 -> 59 -> 60 -> 61 -> 62 -> 63 -> 64 -> 65 -> 66 -> 67 -> 68 -> 69 -> 70 -> 71 -> 72 -> 72 |}]
