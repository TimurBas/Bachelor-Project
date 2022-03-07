open Types

type bas_val = Int of int | Bool of bool | String of string

type exp =
  | BasVal of bas_val
  | Var of program_variable
  | Lambda of { id : program_variable; e1 : exp }
  | App of { e1 : exp; e2 : exp }
  | Let of { id : program_variable; e1 : exp; e2 : exp }
  | Tuple of { e1 : exp; e2 : exp }
  | Fst of exp
  | Snd of exp
