open Types

module Substitution =
  Map.Make (struct 
    type t = string
    let compare = compare
end)

type map_type = typ Substitution.t
let empty: map_type = Substitution.empty
let add k v t : map_type = Substitution.add k v t
let look_up k t: typ option = Substitution.find_opt k t
let remove k t: map_type = Substitution.remove k t 
let merge lst1 lst2: map_type = Substitution.merge 
(
  fun k xo yo -> 
    match xo, yo with 
      | Some x, Some y -> Some (x+y)
      | None, yo -> yo
      | xo, None -> xo
) lst1 lst2