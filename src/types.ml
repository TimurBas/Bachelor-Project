module SS = Set.Make(Int)
module UF = UnionFind

type tycon =
| Int
| Bool
| String

type tyvar = int

type typ = 
| TyCon of tycon
| TyVar of tyvar
| TyFunApp of {t1: typ_node; t2: typ_node}
| TyTuple of {t1: typ_node; t2: typ_node}

and typ_node = typ UF.t

type typescheme = TypeScheme of {tyvars: SS.t; tau_node: typ_node}

type program_variable = string