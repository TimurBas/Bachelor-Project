module SS = Set.Make(Int)

exception Fail of string

type tycon =
| Int
| Bool
| String

type typ = 
| TyCon of tycon
| TyVar of tyvar ref
| TyFunApp of {t1: typ; t2: typ}
| TyTuple of {t1: typ; t2: typ}

and tyvar = Int of int | Link of typ

type typescheme = TypeScheme of {tyvars: SS.t; tau: typ}

type program_variable = string

let rec find typ: typ = match typ with
| TyVar ({contents = Link t} as kind) ->
  let root = find t in
  kind := Link root;
  root
| _ -> typ

let union tv t = tv := Link (find t)