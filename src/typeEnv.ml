open Types

exception Impossible

module Gamma =
  Map.Make (struct 
    type t = program_variable
    let compare = compare
end)

let wrap_monotype tau = TypeScheme {tyvars=SS.empty; tau}

type map_type = typescheme Gamma.t

let empty: map_type = Gamma.empty

let add k v t : map_type =
  Gamma.add k v t

let add_alpha k v t: typescheme Gamma.t = 
  Gamma.add k (wrap_monotype v) t

let look_up k t: typescheme option = Gamma.find_opt k t

let remove k t: map_type = Gamma.remove k t

let bindings t = Gamma.bindings t

let map m (t: map_type) = Gamma.map m t