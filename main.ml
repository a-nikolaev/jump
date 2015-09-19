
open Base
open Ast

open Printf


let non_interactive ic finalize =
  let lexbuf = Lexing.from_channel ic in

  let ls, ht = Parser.program Lexer.token lexbuf in

  printf "\n\n=\n\n";

  List.iter (fun decl -> 
      let s = s_of_decl ht 0 decl in
      printf "%s%!" s
    )
    ls;

  finalize();
  
  printf "\n\n=\n\n";

  try
    Eval.run_program ht ls
  with
    Eval.RT_NotFound n -> eprintf "Runtime error: The name '%s' is not found.\n" (Hashtbl.find ht n) 

let _ = 
  if Array.length Sys.argv > 1 then 
    let ic = open_in Sys.argv.(1) in 
    non_interactive ic (fun () -> close_in ic)
  else
    non_interactive stdin (fun () -> ())

