(* 11/05/2007 *)
(* Static analysis of BNG systems*)
(* Jerome Feret pour PlectiX *)
(* ToolKit *)
(* tools.ml *)

(** Various primitives *)

open Kappa_parse


let log_in_file file dump = 
  (** This function allows the dumping of text in a file 
      When applied, a file (specified thanks to the first argument) is opened, then the text is dump (by appying the second argument with the channel associated with the file, last the file is closed *)
   if file = "" then () 
   else 
   begin
   let output = open_out file in 
   let _ = dump output in 
   let _ = close_out output in 
   () 
   end 

let open_file file = 
   (** This primitives open a file, when necessary.
       This primitive takes a string and return an optional channel.
       Whenever the string is empty, it return None, otherwise it returns the corresponding channel *)
  if file = "" then None
  else Some (open_out file)

let close_file chan =
   (** This primitive performs the converse operation: if a file has been opened, it closes this file, otherwise it does nothing.*)
   match chan with None -> ()
   | Some a -> close_out a
   
  
