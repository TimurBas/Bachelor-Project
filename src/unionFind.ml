type 'a node = 
  | NoLink of 'a
  | Link of 'a t 
and 'a t = 'a node ref

let make_set data = ref (NoLink data)

let rec find node = match node with
  | Link node' -> find !node'
  | NoLink t -> t

let rec root node = match !node with
  | Link node' -> root node'
  | NoLink _ -> node

let link (n1: 'a node ref) (n2: 'a node ref) =
  match !n1, !n2 with 
    | NoLink _, NoLink _ -> n1 := Link n2
    | _ -> raise (Failure "Both are not roots")

let union n1 n2 =
  let a = root n1 in
  let b = root n2 in
  link a b