# Mini-ML Interpreter

This project is carried out as part of the third year of functional programming at the University of Paris-Saclay in mathematics and computer science.
The objective of the project is the construction of an interpreter for a fragment of Caml. The interpreter combines functional aspects and mutable data structures. The project consists of four files, namely mmllexer.mll, mmlparser.mly, typechecker.ml and interpreter.ml. The files must be completed to produce the final program, mmli, which will be a complete interpreter for Mini-ML. 

# Code
The following code defines two types, intref and sequence, and two functions, max_value and syracuse, which will be used to test the interpreter.

    type intref = { mutable value: int; }
    type sequence = { start: int; next: int -> int; stop: int -> bool; }

    let max_value (s:sequence) =
        let max = { value = s.start; } in
        let rec iter (n:int): unit =
            if max.value < n then max.value <- n;
            if not (s.stop n) then iter (s.next n)
        in
        iter s.start;
        max.value
    in

    let syracuse n = {
        start = n;
        next = (fun (k:int) -> if k mod 2 == 0 then k/2 else 3*k+1);
        stop = (fun (k:int) -> k <= 1);
    }
    in

    max_value (syracuse 27)


# Syntax

The syntax of Mini-ML is defined by the following rules:

    <program> ::=  [<type_def>]* <expr> eof

    <type_def> ::=  type ident = { [[mutable]? ident : <type> ;]+ }

    <type> ::=  int 
            |   bool
            |   unit
            |   ident
            |   <type> -> <type>
            |   ( <type> )

    <expr> ::=  <s_expr>
            |   <uop> <s_expr>
            |   <expr> <bop> <expr>
            |   <expr> <s_expr>
            |   if <expr> then <expr>
            |   if <expr> then <expr> else <expr>
            |   fun ( ident : type ) -> <expr>
            |   let ident [( ident : <type> )]* = <expr> in <expr>
            |   let rec ident [( ident : <type> )]* : <type> = <expr> in <expr>
            |   <s_expr> . ident <- <expr>
            |   <expr> ; <expr>

    <s_expr> ::=  n 
            |   true 
            |   false
            |   ()
            |   ident
            |   <s_expr> . ident
            |   { [ident = <expr> ;]+ }
            |   ( <expr> )

    <uop> ::=  -  |  not
    <bop> ::=  +  |  -   |  *  |  /   |  mod
            |  ==  |  !=  |  <  |  <=  |  &&   |  ||

# Abstract Syntax

The following types define the abstract syntax of Mini-ML:

    type prog = (string * typ) list * expr

    and expr =
    | Eint of int
    | Ebool of bool
    | Eunit
    | Eident of string
    | Ebinop of bop * expr * expr
