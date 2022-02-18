open Src

module T = Test

let () =
  let result = T.add 2 3 in
  print_endline (string_of_int result)
