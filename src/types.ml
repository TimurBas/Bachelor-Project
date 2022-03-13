module SS = Set.Make(Int)

type tyvar = int

type tycon =
| Int
| Bool
| String

type typ = 
| TyCon of tycon
| TyVar of tyvar
| TyFunApp of {t1: typ; t2: typ}
| TyTuple of {t1: typ; t2: typ}

type typescheme = 
| TypeScheme of {tyvars: SS.t; tau: typ}  

(* type types = Type of typ | TypeScheme of typescheme *)

type program_variable = string