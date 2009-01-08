(* 2009/01/08*)
(* Meta language for Kappa *)
(* Jerome Feret*)
(* Configuration *)
(* config_metaplx.ml *)

open Data_structures 
open Superarg
open SuperargTk

let ad2 d = 
  if d>= 0 && d<=9 then "0"^(string_of_int d)
  else string_of_int d
    
let time_stamp = 
  let tm = Unix.localtime (Unix.time ()) 
  in
  Printf.sprintf "%s/%s/%d (%s:%s:%s)" 
    (ad2 (tm.Unix.tm_mon+1)) 
    (ad2 tm.Unix.tm_mday) 
    (tm.Unix.tm_year + 1900)
    (ad2 tm.Unix.tm_hour) 
    (ad2 tm.Unix.tm_min) 
    (ad2 tm.Unix.tm_sec)

let version = "1.."^(string_of_int Svn_number.svn_number) 
let date = "2009.08.06"
let input_marshalling = ref "" 
let input_file = ref [""] 
let input_focus_on = ref ""
let refine_fic = ref ""
let key = ref "0000000000000000" 


(* Trace *)
let dump_chrono = ref true 
let dump_version = ref false 
let trace=ref false (* debug *)
let unsafe_mode=ref false (*debug *)
let memory_limit = ref 0 



let output_file = ref ""
let keep_comments = ref true 
let simplxname = "SIMulator by PLectiX:  simplx "^Data.version
let complxname = "COMpressor by PLectiX: complx "^version 
let metaname   = "METAlanguage: metaplx "^version

let sepname = "\n"
let headline = ["This file has been automatically computed by the METAlanguage frontend ";metaname]

let head = ref ("\n\n ******************************************************************************************\n 
"^(List.fold_right (fun a b -> a^"\n"^b) headline "")^
"******************************************************************************************\n")
let foot = head

let options = List.rev

[
(*2_Output*)
"--truc2",Void,"help",["1_Output"],Normal;
"--output",String output_file,"where to dump the result",["1_Output"],Normal;

"--memory-limit",Int memory_limit,"Limit the usage of the memory in (Mb)",["2_Memory usage"],Normal;

(*Debug *)
   
   "--trace",Bool trace,"to dump debuging information",["3_Debug"],Expert;
   "--unsafe-mode",Bool unsafe_mode,"to keep on computation after unexpected hehavior",["3_Debug"],Expert;
   "--version",Bool dump_version,"to dump version number",["3_Debug"],Normal;

(*key *)

"--key",String key,"security key",["Reachability analysis"],Hidden


    ] 

