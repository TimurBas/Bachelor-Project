module UnionFind = struct
  type 'a node = {parent: 'a node option ref; data: 'a}
  let make_set (data: 'a) = {parent = ref None; data}
  let rec find (node: 'a node) = match !(node.parent) with
    | Some n -> find n
    | None -> node
  let union (n1: 'a node) (n2: 'a node) (f: 'a -> 'a -> int) =
    let a = find n1 in
    let b = find n2 in
    let parent = f a.data b.data in
    if parent = 0 then
      b.parent := Some a
    else
      a.parent := Some b
end