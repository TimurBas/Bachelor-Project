open Types

module Gamma =
  Map.Make (struct 
    type t = program_variable
    let compare = compare
end)

type map_type = typescheme Gamma.t
let empty: map_type = Gamma.empty
let add k v t : map_type = Gamma.add k v t
let look_up k t: typescheme option = Gamma.find_opt k t
let remove k t: map_type = Gamma.remove k t