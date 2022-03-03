module A = Ast
let non_polymporphic_id_example = A.Lambda {id = "x"; e1 = A.Lambda {id = "y"; e1 = A.Lambda {id = "z"; e1 = Var "z"}}}

let polymorphic_id_example = A.Let {id = "id"; 
                                   e1 = A.Lambda {id = "x"; e1 = A.Var "x"};
                                   e2 = A.Var "id"}

let let_example = 
  A.Let {
  id = "id";
  e1 = A.Lambda{id = "x"; e1 = A.Var "x"};
  e2 = 
    A.Let{
    id = "both";
    e1 =A.Lambda{id = "y"; e1 = A.Lambda{id="z"; e1= A.Var "id"}}; 
    e2= A.Let{
      id = "amk"; 
      e1 = A.Lambda{id="x"; e1 = A.Var "both"};
      e2 = A.Tuple{e1=A.Var "amk"; e2=A.Var "amk"}}}}

let fun_application_example = 
  A.Lambda {id = "x"; e1 = A.Lambda {id = "y"; e1 = A.App {e1 = A.Var "x"; e2 = A.Var "y"}}}

let fun_application_three_example = 
  A.Lambda {id = "x"; e1 = A.Lambda {id = "y"; e1 = A.Lambda {id = "z"; e1 = A.App {e1 = A.Var "z"; e2 = A.App {e1 = A.Var "x"; e2 = A.Var "y"}}}}}

let big_small_ass_example = 
  A.Lambda {id = "x"; e1 =
    A.Let {
      id = "y"; 
      e1 = A.App {e1 = A.Lambda{id = "w"; e1 = A.Var "w"}; e2 = A.Var "x"};
      e2 = A.Lambda {id = "u";
                     e1 = A.Lambda {id = "z"; 
                                    e1 = A.Tuple {e1 = A.App {e1 = A.Var "y"; 
                                                              e2 = A.Var "u"}; 
                                                              e2 = A.App {e1 = A.Var "y"; 
                                                                          e2 = A.Var "z"}}}}}}