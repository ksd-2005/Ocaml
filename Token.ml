open Grammar
open Ast

type token = Grammar.token = 
  | VAR of string
  | INT_TYPE
  | BOOL_TYPE
  | FLOAT_TYPE
  | VECTORI_TYPE
  | VECTORF_TYPE
  | MATRIXI_TYPE
  | MATRIXF_TYPE
  | PRINT
  | INPUT
  | STRING of string
  | INT of int
  | FLOAT of float
  | BOOL of bool
  | PLUS
  | SUB
  | MUL
  | DIV
  | EQ
  | GT
  | LT
  | GEQ
  | LEQ
  | NEQ
  | AND
  | OR
  | NOT
  | LOOP
  | WHILE
  | DO
  | DONE
  | IF
  | THEN
  | ELSE
  | LPREN
  | RPREN
  | LSQU
  | RSQU
  | LBRA
  | RBRA
  | COMMA
  | SEMICOL
  | ASSGN
  | DIM
  | ABS
  | MOD
  | MAG
  | SQRT
  | ANGLE
  | TRACE
  | TRANSMAT
  | DET
  | INVERSE
  | VECTORI of Ast.expr list
  | VECTORF of Ast.expr list
  | MATRIXI of Ast.expr list
  | MATRIXF of Ast.expr list
  | DOLL
  | EOF
  | ERROR


let rec pp_expr = function
  | Int i -> Printf.sprintf "Int(%d)" i
  | Float f -> Printf.sprintf "Float(%f)" f
  | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
  | Var s -> "Var(" ^ s ^ ")"
  | Vectori es -> "Vectori([" ^ String.concat "; " (List.map pp_expr es) ^ "])"
  | Vectorf es -> "Vectorf([" ^ String.concat "; " (List.map pp_expr es) ^ "])"
  | Matrixi es -> "Matrixi([" ^ String.concat "; " (List.map pp_expr es) ^ "])"
  | Matrixf es -> "Matrixf([" ^ String.concat "; " (List.map pp_expr es) ^ "])"
  | DualOp (op, e1, e2) ->
      "DualOp(" ^ op ^ ", " ^ pp_expr e1 ^ ", " ^ pp_expr e2 ^ ")"
  | SinOp (op, e) ->
      "SinOp(" ^ op ^ ", " ^ pp_expr e ^ ")"
| String s -> Printf.sprintf "String(\"%s\")" s
  



  let rec pp_token = function
  | BOOL b             -> "BOOL(" ^ string_of_bool b ^ ")"
  | INT i              -> Printf.sprintf "INT(%d)" i
  | FLOAT f            -> Printf.sprintf "FLOAT(%f)" f
  | VECTORI ts          -> "VECTORI([" ^ String.concat "; " (List.map pp_expr ts) ^ "])"
  | VECTORF ts          -> "VECTORF([" ^ String.concat "; " (List.map pp_expr ts) ^ "])"
  | MATRIXI ts          -> "MATRIXI([" ^ String.concat "; " (List.map pp_expr ts) ^ "])"
  | MATRIXF ts          -> "MATRIXF([" ^ String.concat "; " (List.map pp_expr ts) ^ "])"
  | INT_TYPE           -> "INT_TYPE"
  | BOOL_TYPE          -> "BOOL_TYPE"
  | FLOAT_TYPE         -> "FLOAT_TYPE"
  | VECTORI_TYPE        -> "VECTORI_TYPE"
  | VECTORF_TYPE        -> "VECTORF_TYPE"
  | MATRIXI_TYPE        -> "MATRIXI_TYPE"
  | MATRIXF_TYPE        -> "MATRIXF_TYPE"
  | VAR s              -> "VAR(" ^ s ^ ")"
| STRING s           -> Printf.sprintf "STRING(\"%s\")" s
  | NOT                -> "NOT"
  | AND                -> "AND"
  | OR                 -> "OR"
  | PLUS               -> "PLUS"
  | MUL                -> "MUL"
  | SUB                -> "SUB"
  | DIV                -> "DIV"
  | ABS                -> "ABS"
  | EQ                 -> "EQ"
  | GT                 -> "GT"
  | LT                 -> "LT"
  | LEQ                -> "LEQ"
  | GEQ                -> "GEQ"
  | NEQ                -> "NEQ"
  | MOD                -> "MOD"
  | MAG                -> "MAG"
  | SQRT               -> "SQRT"
  | DIM                -> "DIM"
  | TRANSMAT           -> "TRANSMAT"
  | DET                -> "DET"
  | INVERSE            -> "INVERSE"
  | ANGLE              -> "ANGLE"
  | TRACE              -> "TRACE"
  | ASSGN              -> "ASSGN"
  | SEMICOL            -> "SEMICOL"
  | LBRA               -> "LBRA"
  | RBRA               -> "RBRA"
  | IF                 -> "IF"
  | THEN               -> "THEN"
  | ELSE               -> "ELSE"
  | LSQU               -> "LSQU"
  | RSQU               -> "RSQU"
  | COMMA              -> "COMMA"
  | LOOP               -> "LOOP"
  | WHILE              -> "WHILE"
  | LPREN              -> "LPREN"
  | RPREN              -> "RPREN"
  | DO                 -> "DO"
  | PRINT              -> "PRINT"
  | INPUT              -> "INPUT"
  | ERROR              -> "ERROR"
  | DOLL                -> "DOLL"
  | DONE               -> "DONE"
  | EOF                -> "EOF"
