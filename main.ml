open Src

module A = Ast
module T = Types

exception Fail

let polymorphic_id_example = A.Let{id = "id"; 
                                   e1 = A.Lambda{id = "x"; e1 = A.Var"x"};
                                   e2 = A.Var "id"}

let unfold_typescheme (typescheme: T.typescheme): T.typescheme_compact = raise Fail
                                   
let algorithm_w (exp: A.exp) = 
  let trav (gamma: TypeEnv.map_type) exp =
    match exp with 
    | A.Var id -> (match TypeEnv.look_up id gamma with 
      | Some typescheme -> 
        let unfolded_typescheme = unfold_typescheme typescheme in 
        raise Fail
      | None -> raise Fail
      )
    | _ -> 3
in trav TypeEnv.empty exp
