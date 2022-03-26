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

let rec find typ = match typ with
  | TyCon _ -> typ
  | TyVar {contents = Link t} -> find t
  | TyVar _ -> typ
  | TyFunApp {t1; t2} -> TyFunApp {t1 = find t1; t2 = find t2}
  | TyTuple {t1; t2} -> TyTuple {t1 = find t1; t2 = find t2}

let union tv t = tv := Link t