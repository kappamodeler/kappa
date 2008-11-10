(* error_handler_common.ml for complx/simplx *)
(* 08/07/2008 *)
(* Jerome Feret for Plectix *)

(** This module provides common  primitives to catch exceptions and store them (with contextal information) in a stack  *)

open Exceptions


(** Either Complx or Simplx *)
let application_name = ref (None:string option)

(** this reference should contain the name of the method that is currently run *)
let method_name = ref (None:string option)

(** this reference should contain the name of the ml file that is currently run*)
let file_name = ref (None:string option)

(** this reference should contain the name of the function that is currently computed*)
let function_name = ref (None:string option)

(** this reference should contain the list of method call in the pipeline *)
let calling_stack = ref (None:string list option)

(** this reference should contain a description of the error *)
let message = ref (None:string option)

(** this reference should contain a key (such as a line number) for the error  *)
let ind = ref (None:string option)

(****************************************)
(* Function to update error description *)
(****************************************)

(** to update the application name *)
let set_app name = application_name:=Some name
let reset_app () = application_name:=None 


(** to update the name of the current ml file *)
let set_file_name name = 
  file_name:= Some name

(** to reset the name of the current ml file *)
let reset_file_name () = 
  file_name:= None 

(** to update the name of the computed function *)
let set_function_name name = 
  function_name:= Some name

(** to reset the name of the computed function *)
let reset_function_name name =
  function_name:= None 

(** to update the calling stack *)
let set_calling_stack l = 
  calling_stack:= Some l 

(** to reset the calling stack *)
let reset_calling_stack () = 
  calling_stack:= None

(** to update the prompted message *)
let set_message m = 
  message:= Some m 

(** to reset the prompted message *)
let reset_message () = 
  message:= None

(** to update the key of errors *)
let set_key l = 
  ind := Some l

(** to reset the key for errors *)
let reset_line () = 
  ind := None 


(*********************)
(* Type declaration  *)   
(*********************)

(** type for errors *)
type error = 
    {application:string option ;
     method_name:string option; 
     file_name:string option ;
     function_name:string option;
     calling_stack:string list option ;
     message:string option ;
     key:string option ;
     exception_:exn}

(** this reference contains a stack of errors *)
let error_list = ref ([]:error list) 

(** this function adds an error to the list *)
let add_error e = error_list:= (e::(!error_list))

(** empty the stack of error *)
let reset_error_list () = error_list:=[] 

(** update the calling stack
    prefix should contain a string (to be prompted before logging information)
                  and a calling stack

    suf contains the called method *)
let add_suffix prefix suf = 
  let pref = "-"^(fst prefix) in
  let call = if suf = "" then snd prefix else suf::(snd prefix) in
  let _ = set_calling_stack call in
  (pref,call) 
  
(** Exception declaratiob for the exceptions that are caught by the error handlers *)
exception Exception of error
