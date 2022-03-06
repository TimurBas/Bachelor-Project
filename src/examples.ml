open Ast
let non_polymporphic_id_example = Lambda {id = "x"; e1 = Lambda {id = "y"; e1 = Lambda {id = "z"; e1 = Var "z"}}}

let polymorphic_id_example = Let {id = "id"; 
                                   e1 = Lambda {id = "x"; e1 = Var "x"};
                                   e2 = Var "id"}

let nested_let_example = 
  Let {
  id = "id";
  e1 = Lambda{id = "x"; e1 = Var "x"};
  e2 = 
    Let{
    id = "both";
    e1 =Lambda{id = "y"; e1 = Lambda{id="z"; e1= Var "id"}}; 
    e2= Let{
      id = "amk"; 
      e1 = Lambda{id="x"; e1 = Var "both"};
      e2 = Tuple{e1=Var "amk"; e2=Var "amk"}}}}

let fun_application_example = 
  Lambda {id = "x"; e1 = Lambda {id = "y"; e1 = App {e1 = Var "x"; e2 = Var "y"}}}

let fun_application_three_example = 
  Lambda {id = "x"; e1 = Lambda {id = "y"; e1 = Lambda {id = "z"; e1 = App {e1 = Var "z"; e2 = App {e1 = Var "x"; e2 = Var "y"}}}}}

(* let everything_example = 
  A.Lambda {id = "x"; e1 =
    A.Let {
      id = "y"; 
      e1 = A.App {e1 = A.Lambda{id = "w"; e1 = A.Var "w"}; e2 = A.Var "x"};
      e2 = A.Lambda {id = "u";
                     e1 = A.Lambda {id = "z"; 
                                    e1 = A.Tuple {e1 = A.App {e1 = A.Var "y"; 
                                                              e2 = A.Var "u"}; 
                                                              e2 = A.App {e1 = A.Var "y"; 
                                                                          e2 = A.Var "z"}}}}}} *)
                                                                          
let everything_example = 
  Lambda {id = "x"; e1 =
    Let {
      id = "y"; 
      e1 = App {e1 = Lambda{id = "w"; e1 = Var "w"}; e2 = Var "x"};
      e2 = Lambda {id = "u";
                     e1 = Lambda {id = "z"; 
                                    e1 = Tuple {e1 = App {e1 = Var "y"; 
                                                              e2 = Var "u"}; 
                                                              e2 = App {e1 = Var "y"; 
                                                                          e2 = Var "z"}}}}}}                                                                      

let tuple_fun_application_example = 
  Lambda {
    id = "x";
    e1 = 
      Lambda {
                id = "y"; 
                e1 = 
                  Tuple {
                      e1 = App {e1 = Var "x"; e2 = Var "y"}; 
                      e2 = App {e1 = Var "x"; e2 = Var "y"}}}}         
                      
                      
let lambda_outside_let_example = 
  Lambda {
    id = "x"; 
    e1 = 
      Lambda {
        id = "z";
        e1 = Let {
          id = "y"; 
          e1 = Lambda {
                        id = "w"; 
                        e1 = Tuple {
                          e1 = App {e1 = Var "w"; e2 = Var "x"};
                          e2 = App {e1 = Var "w"; e2 = Var "x"}
                        }}; 
          e2 = Var "y"}
      }}