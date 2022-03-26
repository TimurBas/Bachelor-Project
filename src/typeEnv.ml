open Types

module Gamma =
  Map.Make (struct 
    type t = program_variable
    let compare = compare
end)

type ts_map = typescheme Gamma.t

let wrap_monotype tau = TypeScheme {tyvars=SS.empty; tau}

let empty = Gamma.empty

let add k v t = Gamma.add k v t

let look_up k t = Gamma.find_opt k t

let remove k t = Gamma.remove k t

let bindings t = Gamma.bindings t

let map m t = Gamma.map m t

let update_typescheme (TypeScheme {tyvars; tau}) =
  TypeScheme {tyvars; tau = find tau}
let update_typeschemes t = map update_typescheme t

let counter = ref 0
let get_next_tyvar () =
  counter := !counter + 1;
  !counter
let reset () = counter := 0