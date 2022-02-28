type typ = 
| TyCon of string
| TyVar of string 
| TyFunApp of {t1: string; t2: string}
| TyTuple of {t1: string; t2: string}

type typescheme = 
| Type of typ 
| TypeScheme of {tyvar: string; typescheme: typescheme}   

type typescheme_compact = 
| CompactTypeScheme of {tyvars: string list; typ: typ}

type program_variable = string