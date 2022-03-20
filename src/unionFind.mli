type 'a node = {parent: 'a node option ref; data: 'a}
val make_set : 'a -> 'a node
val find : 'a node -> 'a node
val union : 'a node -> 'a node -> ('a -> 'a -> int) -> unit