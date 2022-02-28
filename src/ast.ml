open Types

type exp =
| Let of {id: program_variable; e1: exp; e2: exp} 
| Lambda of {id: program_variable; e1: exp} 
| App of {e1: exp; e2: exp}
| Var of program_variable