type exp =
| Let of {id: string; e1: exp; e2: exp} 
| Lambda of {id: string; e1: exp} 
| App of {e1: exp; e2: exp}
| Var of string