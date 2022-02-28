type tau = 
| TyCon of string
| TyVar of string 
| TyFunApp of {tau1: string; tau2: string}
| TyTuple of {tau1: string; tau2: string}

type typescheme = 
| Type of tau 
| TypeScheme of {tyvars: tau.TyVar; tau: tau}   

type program_variable = string