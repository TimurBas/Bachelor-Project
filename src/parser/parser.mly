%{
    open Common.Ast
%}

%token EOF
%token <string> ID
%token <int> INT 
%token <string> STRING 
%token LET IN FUN ASSIGN LPAREN RPAREN COMMA RARROW FST SND

%start <Common.Ast.exp> program  

%%
program: e = exp EOF                            { e }

exp:
| i=INT                                         {BasVal(Int i)}
| s=STRING                                      {BasVal(String s)}  
| s=ID                                          {Var s}    
| FUN s=STRING RARROW e1=exp                    {Lambda {id=s; e1}}
| LET id=ID ASSIGN e1=exp IN e2=exp             {Let {id; e1; e2}}
| e1=exp e2=exp                                 {App {e1; e2}}
| LPAREN e1=exp COMMA e2=exp RPAREN             {Tuple {e1; e2}}
| FST e1=exp                                    {Fst e1}
| SND e1=exp                                    {Snd e1}