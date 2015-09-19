%{ 
  open Ast 

  let s2i_ht = Hashtbl.create 128
  let i2s_ht = Hashtbl.create 128 

  let get_name s = 
    try Hashtbl.find s2i_ht s with
    | Not_found -> 
      ( let n = Hashtbl.length s2i_ht in
        Hashtbl.replace s2i_ht s n;
        Hashtbl.replace i2s_ht n s;
        n 
      )
%}

%token EOF

%token OPAR CPAR
%token <string> NAME
%token <string> LABEL
%token <int> INT
%token BAR
%token ARROW
%token JUMP
%token EQUALS
%token LET
%token IN
%token WHERE
%token FUN
%token END

%token COMMA

%token TRUE
%token FALSE

%token PLUS
%token MINUS
%token EQUALS2
%token LESS  
%token GREATER  
%token LESSEQ  
%token GREATEREQ  

%left PLUS  
%left MINUS  
%left EQUALS2
%left LESS  
%left GREATER  
%left LESSEQ 
%left GREATEREQ  

%start program
%type <(Ast.decl list * (int,string) Hashtbl.t)> program

%%

program :
    | decl_list EOF { ($1, i2s_ht) }
    ;
    
decl_list :
    | list (decl) { $1 } 
    ;

decl :
    | LET pat EQUALS expr { DLet ($2, $4) }
    ;

pat :  
    | INT  {PLit (LInt $1)}
    | TRUE {PLit (LBool true)}
    | FALSE {PLit (LBool false)}
    | NAME {PName (get_name $1)}
    | OPAR z = separated_list(COMMA, pat) CPAR 
        { match z with
          | [] -> PTuple []
          | [x] -> x
          | ls -> PTuple ls
        } 
    ;

expr :
    | JUMP label WHERE dispatch END {EJumpIn ((Some $2, None), $4)}
    | JUMP expr_x WHERE dispatch END {EJumpIn ((None, Some $2), $4)}
    | JUMP label expr_x WHERE dispatch END {EJumpIn ((Some $2, Some $3), $5)}
    
    | JUMP label {EJump (Some $2, None)}
    | JUMP expr_x {EJump (None, Some $2)}
    | JUMP label expr_x {EJump (Some $2, Some $3)}
    
    | LET pat EQUALS expr IN expr {ELetIn ($2, $4, $6)}
    | FUN pat ARROW expr {EFun ($2, $4)}
    | expr_a { $1 }
    ;
    

expr_a :
    | expr_a PLUS expr_a {EBinOp(BinOpPlus, $1, $3)}
    | expr_a MINUS expr_a {EBinOp(BinOpMinus, $1, $3)}
    | expr_a EQUALS2 expr_a {EBinOp(BinOpEquals, $1, $3)}
    | expr_a LESS expr_a {EBinOp(BinOpLess, $1, $3)}
    | expr_a GREATER expr_a {EBinOp(BinOpGreater, $1, $3)}
    | expr_a LESSEQ expr_a {EBinOp(BinOpLessEq, $1, $3)}
    | expr_a GREATEREQ expr_a {EBinOp(BinOpGreaterEq, $1, $3)}
    | ls = nonempty_list(expr_x) 
        { match ls with
          | [hd] -> hd
          | hd :: tl -> List.fold_left (fun acc next -> EApp (acc, next)) hd tl
          | [] -> failwith "impossible: empty list parsed"
        }

expr_x :  
    | ename {$1}
    | elit {$1}
    | OPAR z = separated_list(COMMA, expr) CPAR 
        { match z with
          | [] -> ETuple []
          | [x] -> x
          | ls -> ETuple ls
        } 
    ;

label :
    | LABEL {get_name $1}

ename :    
    | NAME {EName (get_name $1)}
    ;

elit :    
    | INT {ELit (LInt $1)}
    | TRUE {ELit (LBool true)}
    | FALSE {ELit (LBool false)}
    ;

dispatch :
    | nonempty_list (branch) {$1}
    | OPAR dispatch CPAR {$2}
    ;

branch :
    | BAR label pat ARROW expr {(Some $2, Some $3, $5)}
    | BAR label ARROW expr {(Some $2, None, $4)}
    | BAR pat ARROW expr {(None, Some $2, $4)}
    ;

%%
