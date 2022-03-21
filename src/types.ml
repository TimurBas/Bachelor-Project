module SS = Set.Make(Int)
module UF = UnionFind

type tycon =
| Int
| Bool
| String

type typ = 
| TyCon of tycon
| TyVar of tyvar
| TyFunApp of {t1: typ; t2: typ}
| TyTuple of {t1: typ; t2: typ}

and tyvar = LinkTo of typ ref | Int of int

type typescheme = 
| TypeScheme of {tyvars: SS.t; tau: typ}

type program_variable = string