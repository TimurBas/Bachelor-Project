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
| TyFunApp of {t1: typ UF.node; t2: typ UF.node}
| TyTuple of {t1: typ UF.node; t2: typ UF.node}

type typescheme = 
| TypeScheme of {tyvars: SS.t; tau_node: typ UF.node}

type program_variable = string