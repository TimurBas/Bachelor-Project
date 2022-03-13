type tycon =
| Int
| Bool
| String

type typ = 
| TyCon of tycon
| TyVar of tyvar
| TyFunApp of {t1: typ; t2: typ}
| TyTuple of {t1: typ; t2: typ}

and tyvar =
  (* (kind * int) ref  *)
  kind ref

and kind = 
| NoLink of int
| LinkTo of typ

type typescheme = 
| TypeScheme of {tyvars: int list; tau: typ}  

(* type types = Type of typ | TypeScheme of typescheme *)

type program_variable = string