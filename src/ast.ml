open Types

type exp =
| Var of program_variable
| Lambda of {id: program_variable; e1: exp} 
| App of {e1: exp; e2: exp}
| Let of {id: program_variable; e1: exp; e2: exp}
| Tuple of {e1: exp; e2: exp} 
