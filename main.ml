open Src

module A = Ast
module T = Types

let polymorphic_id_example = A.Let{id = "id"; 
                                   e1 = A.Lambda{id = "x"; e1 = A.Var"x"};
                                   e2 = A.Var "id"}

(* let rec W gamma exp =  match exp with 
  | ... 

in W init_gamma exp *)