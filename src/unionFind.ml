type 'a node = 
  | Root of 'a
  | Link of 'a t 
and 'a t = 'a node ref

let make_set data = ref (Root data)

let rec find node = match !node with
  | Link node' -> find node'
  | Root t -> t

let rec root node = match !node with
  | Link node' -> root node'
  | Root _ -> node

let link (n1: 'a t) (n2: 'a t) =
  match !n1, !n2 with 
    | Root _, Root _ -> n1 := Link n2
    | _ -> raise (Failure "Both are not roots")

let union n1 n2 = link (root n1) (root n2)