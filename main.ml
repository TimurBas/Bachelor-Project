open Src

let () =
  let result = Test.add 2 3 in
  print_endline (string_of_int result)