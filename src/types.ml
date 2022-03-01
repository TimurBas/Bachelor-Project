type typ = 
| TyCon of string
| TyVar of string 
| TyFunApp of {t1: typ; t2: typ}
| TyTuple of {t1: typ; t2: typ}

type typescheme = 
| TypeScheme of {tyvars: string list; tau: typ}  

(* type types = Type of typ | TypeScheme of typescheme *)

type program_variable = string