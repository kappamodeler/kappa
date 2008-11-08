(* Error handlers for complx *)
(* 08/07/2008 *)
(* Jerome Feret for Plectix *)

(** To module provide primitives to catch exceptions and store them (with contextal information) in a stack  *)

open Exceptions

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

(** pretty print exceptions *)
let string_of_exn x = 
  match x with 
    Exit -> "Exit"
  | Not_found -> "Not_found"
  | Memory_overflow -> "Memory_overflow"
  | Arg.Bad x -> "Arg.Bad "^x
  | Sys.Break -> "Sys.Bread"
  | Stack.Empty -> "Stack.Empty"
  | Queue.Empty -> "Queue.Empty"
  | Stream.Error x -> "Stream.Error "^x
  | Stream.Failure -> "Stream.Failure"
  | Arg.Help x -> "Arg.Help "^x
  | Parsing.Parse_error -> "Parsing.Parse_error"
  | Scanf.Scan_failure x -> "Scanf.Scan.failure "^x
  | Lazy.Undefined -> "Lazy.Undefined"
  | UnixLabels.Unix_error _ -> "UnixLabels.Unix_error"
  | Unix.Unix_error _ -> "Unix.Unix.error"
  | Failure x -> ("Failure "^x)
  | Error.Syntax (s,i) -> ("Error.Syntax("^s^","^(string_of_int i)^")")
  | Error.Runtime s -> "Error.Runtime "^s
  | Error.Runtime2 s -> "Error.Runtime2 "^s
  | Error.Found s -> "Error.Found "^s
  | Error.Too_expensive ->  "Error.Too expensive"
  | Error.Not_handled_yet s -> "Error.Not handled yet "^s 
  | _  -> "Not known"


(** handle_errors app meth f a applies f with a,
    in case of exception it stores it in the error stack (filling the name of the application with app, and the name of the method with meth) throw an exception of the form Exception Error *)

let g error = 
  let _ = add_error error in 
  raise (Exception error) 

let compose x y = 
  match x with None -> Some y
  | Some a -> Some (a^y) 

let handle_errors app meth f a = 
  try 
    f a
  with 
    Exception i -> raise (Exception i) 
  | Error.Syntax (s,i) as exn -> 
      g 
	{application = Some "Simplx" ;
	  method_name = meth ;
	  function_name = !function_name ;
	  file_name = Some (!Data.fic) ;
          calling_stack = !calling_stack ;
          message = compose (!message) s ;
          key = compose (!ind) ("line "^(string_of_int i)) ;
          exception_ = exn}
  | Error.Runtime s | Error.Runtime2 s | Error.Found s | Error.Not_handled_yet s as exn ->
      g 
	{application = Some "Simplx"  ;
	  method_name = meth ;
	  function_name = !function_name ;
	  file_name = !file_name;
          calling_stack = !calling_stack ;
          message = compose (!message) s ;
          key = !ind ;
          exception_ = exn}
  | exn -> 
      let error = 
        {application = if exn=Error.Too_expensive then Some "Simplx" else app  ;
	 method_name = meth ;
	  function_name = !function_name ;
	 file_name = !file_name ;
         calling_stack = !calling_stack ;
         message = !message ;
         key = !ind ;
          exception_ = exn}
      in
      let _ = add_error error in 
      raise (Exception error) 

(** handle_errors_return app meth f a b works the same, but it returns b if an exception is raised*)
let handle_errors_return app meth f a b = 
  try 
    handle_errors app meth f a 
  with 
    _ -> b 

(** handle_errors_homo app meth f a works the same, but it returns a if an exception is raised *)
let handle_errors_homo app meth f a = 
  handle_errors_return app meth f a a 

(** handle_errors_main app meth f a works the same, but it returns () if an exception is raised *)
let handle_errors_main app meth f a = 
  let g a b = 
    let _ = f a in () in 
  handle_errors_homo app meth (g a) () 

(** handle_errors_uncurry app meth f a b computes f a b, in case of exceptions, it returns (a,b) and updates the error stack.*)
let handle_errors_uncurry app meth f a b = 
  let g f (a,b) = f a b in
  handle_errors_homo app meth (g f) (a,b)

(** handle_errors_step app meth f prefix pb log computes f prefix pb log, in case of exceptions, it returns (pb,log) and updates the error stack *)
let handle_errors_step app meth f a b c =
  handle_errors_uncurry app meth (f a) b c 

(** handle_errors_def app meth f d prefix pb log computes f prefix pb log, in case of exceptions it returns d,b,c and updates the stack*)
let handle_errors_def app meth f d a b c  = 
  try 
    handle_errors  app meth (f a b)  c 
  with 
    _ -> 
      d,b,c

(** handle_errors_none app meth f a b computes f a b, in case of exceptions, it returns None,b and updates the stack *)
let handle_errors_none app meth f a b = 
  try 
    handle_errors app meth (f a) b 
  with
    _ -> None,b

(** this function convert a handled error into a string *)
let string_of_error error = 
  let string_of_option (s1:string) (s2:string option) = 
    match s2 with 
      None -> ""
    | Some a ->  s1^"=\""^a^"\" " in
  (string_of_option "Application" error.application)
  ^(string_of_option "Method" error.method_name)
  ^(string_of_option "Module" error.file_name)
  ^(string_of_option "Function" error.function_name)
  ^(string_of_option "Message" error.message)
  ^(string_of_option "Key" error.key)
  ^(string_of_option "Exception" (Some (string_of_exn error.exception_)))
    
(** this function dumps a list errors within the XML output *)
let dump_error_in_XML channel  error_list = 
  let print s = Printf.fprintf channel "%s" s in
  let _ = 
    List.iter 
      (fun a -> 
	let _ = print "<Entry Type=\"ERROR\" " in 
	let string_of_int i = 
	      match i with None -> None
	      |	Some a -> Some (string_of_int a) in 
	let print_option (s1:string) (s2:string option) = 
	  match s2 with 
		None -> () 
	  |	Some a -> 
	      let _ = print  s1 in
	      let _ = print "=\"" in
	      let _ = print a in
		  let _ = print "\" " in
		  ()
	in
	let _ = print_option "Application" a.application in
	let _ = print_option "Method" a.method_name in
	let _ = print_option "Module" a.file_name in 
	let _ = print_option "Function" a.function_name  in 
	let _ = print_option "Key" a.key in 
	let _ = print_option "Exception" (Some (string_of_exn a.exception_)) in 
	let _ = 
	      match a.calling_stack 
	      with 
		None -> ()
	      |	Some a -> 
		  let _ = print "Stack=\"" in
		  let _ = 
		    List.fold_left 
		      (fun bool s -> 
			let _ = if bool then print "," in
			let _ = print s in
			true)
		      false (List.rev a) in
		  let _ = print "\" " in 
		  () in
	let _ = print_option "Message" a.message in
	let _ = print "/>\n" in
	()) error_list 
  in () 
	     
let set a b = 
  match a with None -> ()
  | Some a -> b a 

let warn_message warn module_n  function_n k  = 
  (*let _ = set "Complx" set_application *)
  let _ = set warn set_message in 
  let _ = set function_n set_function_name in
  let _ = set module_n set_file_name in
  let _ = set k set_key in
  ()

let unsafe warn module_name function_name key x  = 
   warn_message warn module_name function_name key ;
   if !Config_complx.unsafe_mode then x else raise Exit 

let frozen_unsafe warn module_name function_name key x = 
  warn_message warn module_name function_name key ;
  if !Config_complx.unsafe_mode then x () else raise Exit 

let frozen_exit () = raise Exit 
    
let unsafe_frozen = frozen_unsafe  (*WTF!!*)
