
(* The type of tokens. *)

type token = 
  | UNIT_TYP
  | UNIT
  | TYPE
  | TRUE of (bool)
  | THEN
  | STAR
  | REC
  | PTV
  | PT
  | PLUS
  | PARGAUCHE
  | PARDROITE
  | OU
  | NON
  | NEGAL
  | MUTABLE
  | MOINS
  | MODULO
  | LET
  | INT
  | INFS
  | INF
  | IN
  | IF
  | IDENT of (string)
  | FUN
  | FLECHEG
  | FLECHE
  | FALSE of (bool)
  | ET
  | EOF
  | ELSE
  | EGAL
  | EG
  | DIV
  | DEUXPT
  | CST of (int)
  | CROCHETG
  | CROCHETD
  | BOOL
  | ACCG
  | ACCD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Mml.prog)
