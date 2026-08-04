

open Printf
open Lexing
open Ast
open Typecheck
open Interpreter

let process_file filename =
  try
    let ic = open_in filename in
    let lexbuf = Lexing.from_channel ic in
    try
      let ast = Grammar.program Lexer.token lexbuf in
      Printf.printf "AST:\n%s\n\n" (string_of_ast ast);
      Printf.printf "Type checking...\n";
      try
        typecheck ast;
        Printf.printf "✓ Program type checks successfully!\n";
        Printf.printf "Executing program...\n";
        eval ast;
        Printf.printf "✓ Program executed successfully!\n"
      with TypeError e ->
        Printf.printf "Type Error: %s\n" (string_of_type_error e)
    with
    | Parsing.Parse_error ->
        Printf.fprintf stderr "Syntax error at line %d\n" 
          (Lexing.lexeme_start_p lexbuf).pos_lnum
    | Failure msg -> 
        Printf.fprintf stderr "Execution error: %s\n" msg
  with Sys_error msg ->
    Printf.fprintf stderr "Cannot open file: %s\n" msg

let () =
  if Array.length Sys.argv <> 2 then (
    eprintf "Usage: %s <input file>\n" Sys.argv.(0);
    exit 1
  );
  let filename = Sys.argv.(1) in
  process_file filename

(* use od -a -c A3_tc_1.txt  to see each char in parsing  *)
(*  use dos2unix filename to file to linux supportable one*)
