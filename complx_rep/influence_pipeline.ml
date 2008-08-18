(* 27/02/2007 *)
(* Static analysis of BNG systems*)
(* Jerome Feret pour PlectiX *)
(* Main file: main function + probleme specification *)
(* main.ml *)

open Config_complx
open Tools
open Var
open Expr
open Kleenean_expr
open Abstract_expr_sig
open Data_structures
open Reachability
open Compressor
open Pb_sig 
open Bdd
open Partition
open Rough
open Concretization
open Cbng
open Cbng_sig
open Translate
open Output_contact_map
open Pipeline 


let main ()  = 
  let _ = get_option () in 
  let methods = methods () in 
  let log = methods.empty_channel in 
  let log = methods.print_headpage log in 
  let log = methods.dump_version log in 
  let _ = trace_print "RETURN TR" in 
  let simplx,log = methods.parse_file (List.hd !Config_complx.input_file) log in
  let _ = trace_print "RETURN SIMPLX" in
  let cpb,log = methods.translate simplx "" log in 
  let _ = trace_print "RETURN TRANSLATE" in 
  let pb,log = methods.compile Unsmashed cpb "" log in 
  let pb,log = methods.dump_boolean_encoding pb "" log in 
  let _ = trace_print "RETURN COMPILATION" in 
  let pb,log = methods.build_influence_map pb (!Config_complx.output_influence_map_txt_file) log in 
  let log = methods.dump_session pb (!Config_complx.output_xml) log in 
  let _ = methods.print_channel log in  
  let _ = methods.print_footpage log in 

  match pb with 
      None -> None
    | Some pb -> Some (pb.wake_up_map,pb.inhibition_map)
 



let _ = main () 
