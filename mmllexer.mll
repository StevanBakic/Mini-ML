{

  open Lexing
  open Mmlparser

  exception Lexing_error of string

  let keyword_or_ident =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ "fun" , FUN ;
        "let", LET;
        "rec", REC; 
        "in", IN;
        "if" , IF ;
        "then" , THEN ;
        "else" , ELSE ; 
        "int", INT;
        "bool", BOOL;
        "unit", UNIT_TYP;

        "mod", MODULO;
        "not", NON;
        "type", TYPE;
        "mutable", MUTABLE;


      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)
        
}

let digit = ['0'-'9']
let number = '-'? digit+
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | digit)*
  
rule token = parse
  | ['\n']
      { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
      { token lexbuf }
  | "(*" 
      { comment lexbuf; token lexbuf }
  | number as n
      { CST(int_of_string n) }
  | ident as i 
    { keyword_or_ident i }
  | "()" 
    { UNIT }  
  | "+"
      { PLUS }
  | "*"
      { STAR }
  | "-"
      { MOINS }
  | "/"
      { DIV }
  | "%"
      { MODULO }
  | "=="
      { EGAL }
  | "="
      { EG }    
  | "!="
      { NEGAL }
  | "<"
      { INFS }
  | "<="
      { INF }
  | "&&"
      { ET }
  | "||"
      { OU }
  | "("
      { PARGAUCHE }
  | ")"
      { PARDROITE }
  | "["
      { CROCHETG }
  | "]"
      { CROCHETD }
  | "{"
      { ACCG }
  | "}"
      { ACCD }                        
  | ":"
      { DEUXPT }
  | "."
      { PT }
  | ";"
      {PTV}              
  | "->"
      { FLECHE }
  | "<-"
      { FLECHEG }     


  (*| "not"
      { NON } 
      *)                            
  | _
      { raise (Lexing_error ("unknown character : " ^ (lexeme lexbuf))) }
  | eof
      { EOF }

and comment = parse
  | "*)"
      { () }
  | "(*"
      { comment lexbuf; comment lexbuf }
  | _
      { comment lexbuf }
  | eof
      { raise (Lexing_error "unterminated comment") }
