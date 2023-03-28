%{

  open Lexing
  open Mml

%}

%token <bool> FALSE TRUE  
%token UNIT
%token PARGAUCHE PARDROITE CROCHETG CROCHETD
%token ACCG ACCD
%token PLUS STAR
%token <int> CST
%token <string> IDENT
%token EOF
%token MOINS
%token DIV
%token MODULO
%token EGAL
%token NEGAL
%token INFS
%token INF
%token ET
%token OU
%token IF
%token THEN
%token ELSE
%token FLECHE FLECHEG
%token PT
%token PTV
%token DEUXPT
%token EG

%token NON

%token INT
%token BOOL
%token UNIT_TYP
%token TYPE
%token MUTABLE 

%token FUN LET REC
%token IN

%nonassoc IN ELSE FLECHE
%left ET OU 
%left EGAL INF INFS
%left PLUS
%left STAR DIV
%left PARGAUCHE IDENT CST



%start program
%type <Mml.prog> program

%%

program:
| (*types=list(type_def)*) code=expression EOF { {types=[]; code} }
;

(*
type_def:
| TYPE id=IDENT EG ACCG y1=type_def_param y2=list(type_def_param) ACCD {(id, y1::y2)}
*)

typ:
| INT {TInt}
| BOOL {TBool}
| UNIT {TUnit}
| t1=typ FLECHE t2=typ {TFun(t1,t2)}
| PARGAUCHE t1=typ PARDROITE {t1} 

simple_expression:
| n=CST { Int(n) }
| FALSE {Bool(false)}
| TRUE {Bool(true)}
| UNIT {Unit}
| id=IDENT { Var(id) }
| e1=simple_expression PT id=IDENT {GetF(e1, id)}
| ACCG x=struct_params y=list(struct_params) ACCD {Strct(x::y)}
| pg = PARGAUCHE e1=expression pd = PARDROITE { e1 }
;

expression:
| e=simple_expression { e }
| op=unop e1= simple_expression {Uop(op, e1)}
| e1= expression e2=simple_expression {App(e1, e2)}
| e1=expression op=binop e2=expression { Bop(op, e1, e2) }
| IF e1=expression THEN e2=expression ELSE e3=expression {If(e1,e2,e3)}
| IF e1=expression THEN e2=expression { If(e1, e2, Unit) }
| FUN PARGAUCHE id=IDENT DEUXPT tpx=typ PARDROITE FLECHE e=expression {Fun(id, tpx, e)}
| LET id=IDENT x=list(let_params) EG e1=expression IN e2=expression {Let(id,mk_fun x e1, e2)}
| LET REC id=IDENT x=list(let_params) t1=fun_typ EG e1=expression IN e2=expression {Let(id, Fix(id,(mk_fun_type x t1),( mk_fun x e1)), e2)}
| e1=simple_expression PT id=IDENT FLECHEG e2=expression {SetF (e1, id, e2)}
| e1=expression PTV e2=expression {Seq(e1, e2)}

;
(*
type_def_params:
| id= Ident DEUXPT t=typ {(id,t)}
| MUTABLE id=IDENT DEUXPT t=typ PTV {(id, t)}
*)

fun_typ:
| DEUXPT t1= typ {t1} 

let_params:
| PARGAUCHE id=IDENT DEUXPT t=typ PARDROITE { (id, t) }

struct_params:
| id=IDENT EG DEUXPT e1=expression PTV {(id,e1)}

%inline binop:
| PLUS { Add }
| STAR { Mul }
| MOINS { Sub }
| DIV { Div }
| MODULO { Mod }
| EGAL { Eq }
| NEGAL { Neq }
| INFS { Lt }
| INF { Le }
| ET { And }
| OU { Or }


%inline unop:
| MOINS { Neg }
| NON { Not }


;

