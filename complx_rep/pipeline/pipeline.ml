(* 11/05/2007 *)
(* Static analysis of Kappa systems*)
(* Jerome Feret pour PlectiX *)
(* Main file: main function + probleme specification *)
(* main.ml *)

open Config_complx
open Share
open Tools
open Tools2
open Error_handler_common 
open Error_handler 
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
open Output_contact_map
open Quarkification 
open Count_complexes 
open Contact_map
open Views 
open Ode_computation 
open Find_cycles 
open Connected_components
open Avoid_polymere
open Count_isomorphism 

type prefix = string * string list 
let extend_prefix x = x^"-"

type file_name = string
type simplx_encoding = (Rule.t list * (Solution.t*int)list) option
type 'a intermediate_encoding = 'a Pb_sig.cpb option
type 'a boolean_encoding = 'a Pb_sig.boolean_encoding option 
type 'a internal_encoding = 'a Pb_sig.pb option 
type log = (string*float) list
type message = string list
type output_channel = log*message 

    
type influence_map = IntSet.t IntMap.t * IntSet.t IntMap.t 
type 'a step = prefix -> 'a internal_encoding ->  output_channel -> 'a internal_encoding * output_channel 

type ('a,'b) step_with_output  =
     prefix -> 'a internal_encoding -> output_channel ->  'b * 'a internal_encoding * output_channel
    

let packs = ref ((StringMap.empty):string list list StringMap.t)

type interface = (string*string list*string list)list option 

type 'a pipeline = {
    dump_version: prefix -> output_channel -> output_channel;
    reset:'a step;
    print_footpage: prefix -> output_channel -> output_channel;
    print_headpage: prefix -> output_channel -> output_channel;
    empty_channel:output_channel;
    log_time:prefix -> string -> output_channel -> output_channel;
    add_message:prefix -> string -> output_channel -> output_channel;
    print_channel: prefix -> output_channel -> unit;
    print_errors: prefix -> unit;  
    parse_file:  file_name -> prefix -> output_channel  -> 'a internal_encoding*output_channel ; 
    parse_line_by_line: file_name -> 'a step ;
    unmarshallize: file_name -> prefix -> output_channel -> 'a internal_encoding*output_channel;
    marshallize: file_name -> 'a step;
    build_pb: simplx_encoding -> prefix -> 'a internal_encoding; 
    translate: interface -> 'a step;
    dump_ckappa: file_name -> compile -> 'a step;
    compile: interface -> compile -> 'a step;
    dump_boolean_encoding: file_name -> compile -> 'a step;
    build_contact: precision -> 'a step;
    find_potential_cycles: precision -> 'a step;
    find_connected_components: precision -> 'a step;
    reachability_analysis: 'a step;
    refine_subviews: 'a step;	
    refine_views: 'a step;
    dump_local_views: file_name -> 'a step;
    dump_packs_constraints: file_name -> 'a step;
    dump_contact_map_txt: precision -> file_name -> 'a step;
    dump_contact_map_dot: precision -> file_name -> 'a step;
    dump_contact_map_ps:  precision -> file_name -> 'a step;
    dump_contact_map_jpg: precision -> file_name -> 'a step;
    quarkification: 'a step;
    count_complexes: 'a step;
    build_influence_map:  file_name ->  file_name -> 'a step;
    build_compression: Config_complx.compression_mode -> file_name -> file_name -> 'a step;
    build_enumeration: file_name -> 'a step;
    dump_session: file_name -> 'a step;
    dump_html_output: file_name -> 'a step;
    save_options: 'a step ;
    good_vertice: file_name -> prefix -> output_channel -> StringSet.t option * output_channel ;
    template: file_name -> file_name -> file_name -> file_name -> file_name -> file_name -> file_name -> file_name -> file_name -> file_name -> file_name -> file_name -> file_name -> file_name -> 'a step ;
    dump_potential_cycles: precision -> 'a step;
    refine_system_to_avoid_polymers: 
	file_name -> simplx_encoding option -> Avoid_polymere.mode -> int option -> float  -> ('a,('a rule_class list)) step_with_output; 
    build_drawers: 'a step ; 
    compute_refinement_relation_maximal: 'a step ;
    export_refinement_relation_maximal: ('a,Rule.t list option) step_with_output;
    export_automorphism_number: ('a,Rule.t list option) step_with_output;
    export_refinement_relation_maximal_and_automorphism_number:('a,Rule.t list option) step_with_output; 
compute_refinement_relation_dag: 'a step;	  
    compute_refinement_relation_closure: 'a step;
    dump_maximal_refinement_relation: file_name -> file_name -> 'a step;
    dump_dag_refinement_relation:file_name -> file_name -> 'a step;
    count_automorphisms:'a step;
    dump_latex_dictionary:file_name -> 'a step;
    dump_latex_rule_system:file_name -> 'a step;  
    dump_latex_version:file_name -> 'a step;
    dump_latex_stat:file_name -> 'a step;
    dump_latex_species_number:file_name -> 'a step;
    dump_latex_fragments_number:file_name -> 'a step;
      dump_latex_rules_number:file_name -> 'a step} 


      
let set_packs pb  = 
  (match pb.packs with None -> ()
     | Some i -> Share.packs := i) 
      
module type Pipeline = 
sig 
  type t
  val build: unit -> t pipeline
end





let store_options () = 
  {version=Config_complx.version;
     date=Config_complx.date;
     forward=(!Config_complx.forward);
     wake_up=(!Config_complx.wake_up);
     inhibition=(!Config_complx.inhibition);
     site_abstraction=(!Config_complx.site_abstraction);
     phospho_abstraction=(!Config_complx.phospho_abstraction);
     ignore_linkage=(!Config_complx.ignore_linkage);
     ignore_phospho=(!Config_complx.ignore_phospho);
     auto_packs=(!Config_complx.auto_packs);
     duplicate_rules_when_sym=(!Config_complx.duplicate_rules_when_sym);
     duplicate_rules_when_cycles=(!Config_complx.duplicate_rules_when_cycles)}


let error i x t y = 
    unsafe
      (Some x) 
      (Some "pipeline.ml") 
      (Some t) 
      (Some i) 
      y

let error_frozen i x t y = 
    unsafe_frozen
      (Some x) 
      (Some "pipeline.ml") 
      (Some t) 
      (Some i)
      y

let frozen_error = error_frozen 


module Pipeline = 
  (functor (A:AbstractExprBool) ->
   struct
     module A=A
     module BDD = A 
     module Ite = Iterateur(A)
     module Com = Compressor(A)
     module Ref = Avoid_poly(A)
	 
     let triv_step prefix a c = a,c 

     let build () = 
       
       let rec print_footpage prefix log = (print_string (!Config_complx.foot);log)
       and save_options prefix input (l,m) = 
	 match input with None -> None,(l,m)
	   |Some a -> Some {a with options = Some (store_options ())},(l,m) 
       and reset prefix input (l,m) = 
	 match input 
	 with None -> None,(l,m)
	   |Some a -> 
	      let new_opt = store_options () in 
	      let old_opt = a.options in 
		match old_opt with 
		    None -> Some {pb_init with simplx_encoding = a.simplx_encoding ; txt_lines = a.txt_lines},(l,"Computation has restarted"::m)
		  | Some old_opt ->
		      if change_version new_opt old_opt
		      then 
			Some {pb_init with simplx_encoding = a.simplx_encoding ; txt_lines = a.txt_lines ; quarks=false},(l,"Computation has been restarted"::m)
		      else
			let pb,m = 
			  if change_gathering new_opt old_opt then 
			    {a with gathered_intermediate_encoding = None;
                                    gathered_boolean_encoding = None
			       
			    },"Gathering options have changed"::m 
			  else a,m in 
			let pb,m = 
			  if change_reachability new_opt old_opt then 
			    {pb with packs = None;
			             Pb_sig.reachability_analysis = None;
                                     contact_map = None;
				     concretization = None;
				     pretty_map = StringMap.empty;
			    },"Reachability options have changed"::m 
			  else pb,m in 
			let pb,m = 
			  if change_influence_map new_opt old_opt then 
			    {pb with wake_up_map = None ; inhibition_map = None},"Influence_map options have changed"::m
			  else pb,m
			in Some pb,(l,m)
       and print_headpage prefix log =  (print_string (!Config_complx.head);log) 
       and dump_version prefix (l,m) =
	 if !Config_complx.dump_version 
	 then 
	   (let rep = complxname^sepname^simplxname^"\n"^Key.key_version 
	   in (print_string rep;(l,rep::m)))
	 else (l,m)
       and empty_channel = chrono 
	   empty_prefix 
	   "" [],[]
       and log_time prefix s (l,m) =  (chrono prefix s l,m)
       and add_message prefix s (l,m) = (l,s::m)
       and print_channel prefix (l,m) =
	 let _ = print_newline () in 
	 let _ = print_newline () in 
	 let _ = 
	   if m = [] 
	   then ()
	   else (print_string "Messages: ";
		 print_newline();
		 List.iter (fun x -> print_string x;print_newline ()) m;
		 print_newline ()) in 
	 let _ = dump_chrono prefix l in ()
       and build_pb a prefix = Some {pb_init with simplx_encoding = a}
       and parse_file  s prefix (l,m) =
	 let tmp_forward = !Data.forward in
	 let tmp_mode = !Data.compile_mode in
	 let _ = Data.forward:= (!Config_complx.forward) in 
	 let _ = Data.compile_mode:=true in 
	 let rep =   
	   if s = "" 
	   then None,(l,m)
	   else 
	     let _ = print_option prefix (Some stdout) "Compilation(simplx)\n" in 
	     let _ = add_suffix prefix "Compilation(simplx)\n" in
	     let (a,b,c,d) = Kappa_lex.compile s  in 
	     let b = !Data.init in
	     let _ = trace_print "COMPILATION DONE" in
	     let l = chrono 
		 prefix 
		 "Compilation(simplx)" 
		 l in 
	     (Some {pb_init 
		   with simplx_encoding = (Some (a,b))}
		,(l,m))
	       
	 in 
	 let _ = Data.forward:=tmp_forward in
	 let _ = Data.compile_mode:=tmp_mode in
	 rep
       and good_vertice file prefix (l,m) = 
	 let prefix' = add_suffix prefix "compute the agent occuring in the rules" in 
         let pb,(l,m) = parse_file file prefix' (l,m) in
	 match pb with 
	   None -> None,(l,m)
	 | Some a -> 
	     (match a.simplx_encoding with 
	       None -> None,(l,m)
	     | Some(a,_) -> 
		 Some (List.fold_left 
			 (fun set rule -> 
			   Mods2.IntMap.fold 
			     (fun _ sol ->
			       Solution.AA.fold 
				 (fun _ agent -> StringSet.add (Agent.name agent))
				 sol.Solution.agents
		       )
			     rule.Rule.lhs  
			     (Solution.AA.fold 
				(fun _ agent   -> StringSet.add (Agent.name agent))
				rule.Rule.rhs.Solution.agents  set
				))
			 StringSet.empty a),
		 (let l = chrono prefix "set of Agents occuring in rules " l in l,m)
		     )
	
       and translate interface prefix input (l,m) = 
	 match input
	 with None -> None,(l,m)
	 |Some rep ->
	     match rep.first_encoding, rep.simplx_encoding  with 
	       Some _,_ | _,None -> input,(l,m)
	     | _,Some (a,b) -> 
		 (
		 let _ = add_suffix prefix  "Translation(simplx->ckappa) \n" in
		 let _ = print_option prefix (Some stdout) "Translation(simplx->ckappa)\n" in 
               	 let rep',messages = Translate.translate_rule_list (List.rev a) b interface m in 
		 let _ = trace_print "TRANSLATE DONE" in
		 let l = chrono prefix "Translation" l in 
		 let pb = Some {rep with first_encoding=Some rep'} in
                   pb,(l,m))
       and get_first_encoding interface prefix pb log = 
	 let pb,log = translate interface prefix pb log in
	 match pb 
	 with None -> frozen_error "line 291" "translate has failed" "get_first_encoding" (fun () -> raise Exit)
	 | Some rep -> 
	     (match rep.first_encoding with
	       None ->frozen_error "line 294" "first encoding has failed" "get_first_encoding" (fun () -> raise Exit)
	     | Some a -> pb,log,a )

       and smash_rule_system interface prefix pb (l,m) =
	 let _  = add_suffix prefix "smash_rule_system \n"  in
	 match pb with None -> None,(l,m)
	 | Some rep0 ->
	     (match rep0.gathered_intermediate_encoding with
	       Some _ -> (pb,(l,m))
	     | None -> 
		 (let _ = print_option prefix (Some stdout)  "Quotienting rules\n"  in
		 let pb,(l,m),rep = get_first_encoding interface prefix pb (l,m) in 
		 match pb with None -> None ,(l,m)
		 | Some rep0 -> 
		 let n0 = List.length rep.cpb_rules in 
		 let cpb,m = CBnG.smash_pb true rep  m in 
		 let _ = trace_print "RETURN SMASH_PB" in 
		 let n1 =  (List.length (CBnG.divide_list true rep rep.cpb_rules)) in 
		 let s = "Number of rules   : "^(string_of_int n0)^"\n" in
		 let s2 = "Number of classes: "^(string_of_int n1)^"\n" in
		 let _ = print_option prefix (Some stdout) s in
		 let _ = print_option prefix (Some stdout) s2 in
		 let l = chrono prefix "Quotienting rules" l in 
		 Some {rep0 with gathered_intermediate_encoding  = Some cpb ;
                        n_rules = Some n0 ; 
			n_classes = Some n1},(l,m)))
       and no_smash_rule_system interface prefix pb (l,m) =
	 let prefix'  = add_suffix prefix "no_smash_rule_system"  in
	 match pb with None -> None,(l,m)
	 | Some rep0 ->
	     (match rep0.intermediate_encoding with
	       Some _ -> (pb,(l,m))
	     | None -> 
		 (let _ = print_option prefix (Some stdout)  "Renaming\n"  in
		 let pb,(l,m),rep = get_first_encoding interface prefix' pb (l,m) in 
		 match pb with None -> None ,(l,m)
		 | Some rep0 ->
		    let cpb,m = CBnG.smash_pb false rep  m in 
		    let l = chrono prefix "Renaming" l in
		    let _ = trace_print "RETURN SMASH_PB" in 
		    let pb = Some {rep0 with intermediate_encoding  = Some cpb} in 
		      pb,(l,m)))
       and get_smashed interface prefix pb log =
	 let error i  = frozen_error i "Quotiented system cannot be built" "get_smashed" (fun () -> raise Exit) in
	 let pb,log  = smash_rule_system interface prefix pb log in 
	 match pb with None -> error "line 341"
	 | Some rep0 -> 
	     (match 
	       rep0.gathered_intermediate_encoding
	     with None -> error "line 345"
	     | Some rep -> pb,log,rep
		   )
       and get_no_smashed interface prefix pb log = 
	 let error i  = frozen_error i "Renaming cannot be built" "get_no_smashed" (fun () -> raise Exit) in
	 let pb,log  = no_smash_rule_system interface prefix pb log in 
	 match pb with None -> error "line 351"
	 | Some rep0 -> 
	     match 
	       rep0.intermediate_encoding
	     with None -> error "line 355"
	     | Some rep -> pb,log,rep
       and compile (interface:interface) mode prefix rep (l,m) =
	 let title = 
	   if mode = Smashed 
	   then
	     "Boolean encoding of the quotiented system"
	   else 
	     "Boolean encoding of the initial system"
	 in
	 match rep with None -> (None,(l,m))
	 | Some rep0 -> 
	     let prefix' = add_suffix prefix  title in
	     let _ = print_option prefix (Some stdout) (title^"\n") in 
	     if mode = Smashed 
	     then 
	       match rep0.gathered_boolean_encoding
	       with Some a -> rep,(l,m)
	       | None -> 
		   let pb,(l,m),cpb =  get_smashed 
		       interface 
		       prefix' rep  (l,m) in
		    (
		   match pb with None -> (None,(l,m)) 
		   | Some rep0 -> 
		       let cpb,messages = CBnG.translate_problem rep cpb A.f,m in 
		       let pb = {rep0 with gathered_boolean_encoding = cpb} in
                       let pb = CBnG.refine_contact_map  pb None A.f in 
		       let  pack' = Packing.compute_pack pb in 
		       let pb = {pb with packs = Some pack'} in 
		       let _ =  set_packs pb in 
		       let pb,messages = CBnG.fill_pretty_map  pb A.f,messages in 
		       let l = chrono prefix title l in 
		       ((Some pb),(l,messages)))
	     else
	        match rep0.boolean_encoding
	       with Some a -> rep,(l,m)
	       | None -> 
		   let pb,(l,m),cpb =  get_no_smashed interface prefix' rep (l,m) in
		    (
		   match pb with None -> (None,(l,m)) 
		   | Some rep0 -> 
		       let cpb,messages = CBnG.translate_problem rep cpb A.f,m in 
		       let pb = {rep0 with boolean_encoding = cpb} in
                       let pb = CBnG.refine_contact_map  pb None A.f in 
		       let  pack' = Packing.compute_pack pb in 
		       let pb = {pb with packs = Some pack'} in 
		       let _ =  set_packs pb in 
		       let pb,messages = CBnG.fill_pretty_map  pb A.f,messages in 
		       let l = chrono prefix title  l in 
		       ((Some pb),(l,messages)))
       and get_boolean_encoding interface prefix pb log = 
	 let _ = add_suffix prefix "get_boolean_encoding" in
	 match pb.boolean_encoding with 
	   None -> 
	     (let pb0',log' = compile interface Unsmashed prefix (Some pb) log in 
	     match pb0'
	     with None -> frozen_error "line 410" "Boolean encoding cannot be built" "get_boolean_encoding" (fun () -> raise Exit)
	     | Some pb' -> (
		 match pb'.boolean_encoding with 
		   Some a -> pb',log',a
		 | None -> frozen_error "line 414" "Boolean encoding cannot be built" "get_boolean_encoding" (fun () -> raise Exit)))
	 | Some a -> pb,log,a 
       and get_gathered_boolean_encoding interface prefix pb log = 
	 let _ = add_suffix prefix "get_gathered_boolean_encoding"in
	 match pb.gathered_boolean_encoding with 
	   None -> 
	     (let pb0',log' = compile interface Smashed prefix (Some pb) log in 
	     match pb0'
	     with None -> frozen_error "line 422" "Boolean encoding cannot be built" "get_gathered_boolean_encoding" (fun () -> raise Exit)
	     | Some pb' -> (
		 match pb'.gathered_boolean_encoding with 
		   Some a -> pb',log',a
		 | None -> frozen_error "line 426" "Boolean encoding cannot be built" "get_gathered_boolean_encoding"  (fun () -> raise Exit)))
	 | Some a -> pb,log,a 
       and get_intermediate_encoding interface prefix pb log = 
	 let _ =add_suffix prefix "get_intermediate_encoding" in
	 match pb.intermediate_encoding  with 
	   None -> 
	     let pb',log' = compile interface Unsmashed prefix (Some pb) log in 
	     
	     (match pb' 
	     with None -> frozen_error "line 435" "Intermediate encoding cannot be built" "get_intermediate_encoding" (fun () -> raise Exit)
	     |Some pb' -> 
		 (
		 match pb'.intermediate_encoding 
		 with Some a -> pb',log',a
		 |  None -> frozen_error "line 440" "Intermediate encoding cannot be built" "get_intermediate_encoding" (fun () -> raise Exit)))
	 | Some a -> pb,log,a
       and convert_contact prefix rep (l,m) = 
	 let prefix' = add_suffix prefix "convert_contact" in 
	 match rep with None -> (rep,(l,m))
	 | Some rep' ->
	      match rep'.contact_map with 
	       Some _ -> (rep,(l,m))
	     | None -> 
		 
		 (
		  let _ = print_option  prefix (Some stdout) "Low-res contact map\n" in 
		 let rep',(l,m),a = get_intermediate_encoding None prefix' rep' (l,m) in 
		 (
		 match a.cpb_contact 
		 with None -> (Some rep',(l,m))
		 | Some map  -> 
		    
		     let contact = 
		       String2Map.fold 
			     (fun s m sol -> 
			       List.fold_left 
				 (fun sol s2 -> 
				   Contact_map.add_contact a.cpb_with_dots s s2 sol)
				 sol m)
			 map Contact_map.contact_map_init in 
		     let l = chrono prefix "Low-res contact map" l in 
		     Some {rep' with contact_map = Some contact},
		     (l,m)))
     

       and build_contact res prefix rep (l,m) = 
	 let prefix' = add_suffix prefix "build_contact" in
	 match rep with None -> (rep,(l,m))
	 | Some rep' -> 
	     if res = Low 
	     then
	       begin 
		 let pb,log,_ = get_intermediate_encoding None prefix' rep' (l,m)  in 
		 (Some pb),log 
	       end
	     else
	       (
	       if not (is_views rep)
	       then (
		     let pb,log = reachability_analysis prefix' rep  (l,m) in
	             build_contact res prefix pb log)
	       else (
		 (rep,(l,m))))
       and build_drawers prefix rep (l,m) = 
	   let prefix' = add_suffix prefix "build_drawers" in
	   match rep with None -> (rep,(l,m))
	   | Some rep' -> 
	       let rep',(l,m),cpb = get_intermediate_encoding None prefix' rep' (l,m) in 
	       let rep',contact,(l,m) = 
	      	 match rep'.contact_map with 
		   None -> rep',
		     (match cpb.cpb_contact with Some a -> a | None -> 
		       error_frozen 
			 "line 505" 
			 "Syntactic contact map is missing"
			 "build_drawers" 
			 			 (fun _ -> raise Exit)),(l,m) 
		 | Some a -> rep',
		     String2Map.map 
		       (List.map (fun (a,b,c) -> (a,c)))
		       a.link_of_site,(l,m)
	       in
	       (Some {rep' with 
		      drawers = 
		      Some (build_drawer (fun x -> true)
			cpb.cpb_rules contact)},(l,m))
   
       and find_potential_cycles  res prefix rep (l,m) = 
	 let prefix' = add_suffix prefix  "find_potential_cycles" in
	 match rep with None -> (rep,(l,m))
	 | Some rep' ->
	     let n = 
	       match !Config_complx.cycle_depth
	       with x when x<0 -> None
	       | x -> Some x in
	     let pb,log = build_contact res prefix' rep (l,m) in
	     let rep' = find_cycles n pb in
	     match pb with None -> (rep,(l,m))
	     | Some pb -> 
	     Some {pb with potential_cycles = rep'},(l,m)
		   
       and find_connected_components res prefix rep (l,m) = 
	 let prefix'= add_suffix prefix "find_connected_components" in
	 match rep with None -> (rep,(l,m))
	 | Some rep' -> 
	     let pb,(l,m) = build_contact res prefix' rep (l,m) in
	     let rep' = detecte_connected_components pb in
	     let l = chrono prefix "Connected components detection" l in 
	     match pb with None -> (rep,(l,m))
	     | Some pb -> 
		 Some {pb with connected_components = rep'},(l,m) 
 
       and dump_potential_cycles res prefix rep (l,m) = 
	 let prefix' = add_suffix prefix "dump_potential_cycles"  in
	 match rep with None -> (rep,(l,m))
	 | Some rep' -> 
	     let pb,log  = 
	       match rep'.potential_cycles with 
		 None -> build_contact res prefix' rep (l,m) 
	       | _ -> rep,(l,m) 
	     in
	     match pb with 
	       None -> (pb,(l,m))
	     | Some pb' -> 
		 (print_cycles pb'.potential_cycles;
		 pb,log)
		 
       and get_high_res_contact_map prefix pb log = 
	 match pb.contact_map with 
	   None -> 
	     let pb',log' = build_contact High prefix (Some pb) log in 
	     (match pb' with 
	       None -> frozen_error "line 530" "contact map cannot be built" "get_high_res_contact_map"  (fun () -> raise Exit)
	     | Some a -> 
		 a,log',
		 (match a.contact_map 
		 with Some a -> a
		 |  None -> frozen_error "line 535" "contact_map cannot be built" "get_high_res_contact_map" (fun () -> raise Exit)))
	 | Some a -> pb,log,a
       and get_low_res_contact_map prefix pb log = 
	 let pb,log,cpb = get_intermediate_encoding None (add_suffix prefix "get_low_res_contact_map") pb log in
	 pb,log,cpb.cpb_contact
	      
       and convert_low_in_high a = 
	 String2Map.fold 
	   (fun (a,b) l map ->
	     List.fold_left 
	       (fun map (c,d) ->
		 add_contact false (a,b) (c,d) map)
	       map l)
	   a contact_map_init 
       and get_best_res_contact_map prefix pb log = 
	 match pb.contact_map with 
	   None -> 
	     let pb',log',contact = get_low_res_contact_map prefix pb log in
	     (match contact with 
	       None -> frozen_error "line 530" "contact map cannot be built" "get_best_res_contact_map"  (fun () -> raise Exit)
	     | Some a -> pb,log,convert_low_in_high a)
		 	
	 | Some a -> pb,log,a

       and parse_line_by_line file prefix rep (l,m) = 
	 match rep with None -> (None,(l,m))
	 | Some rep -> 
	     let _ = print_option prefix (Some stdout) "Second compilation (pretty printing)\n" in 

	     try ( let txt = 
	       let canal = 
		 (try (open_in file) 
		 with 
		   _ -> print_string "ouverture du fichier impossible ";raise Exit) in
	       
	       (let lexbuf = Lexing.from_channel canal in
	       let result = Yacc.main Lexeur.token lexbuf in
	       (close_in canal;result)) in 
	     let l = chrono prefix "Second compilation(pretty printing)" l in 
	     Some {rep with txt_lines = Some txt},(l,m))
		with _ -> Some rep,(chrono prefix "Second compilation" l,
                                    (("Complx parser failed at line "^(string_of_int (!Data_structures.parser_line))^" !!!"))::m)
       and reachability_analysis prefix (pb':'a Pb_sig.pb option) (l,m) = 
	 let prefix' = add_suffix prefix "reachability_analysis" in 
	 match pb' 
	 with None -> None,(l,m)
	 | Some pb ->
	     (match pb.Pb_sig.reachability_analysis 
	     with 
	       Some _ -> pb',(l,m) 
	     | None -> 
		 let _ = print_option prefix (Some stdout) "Reachability analysis \n" in
		 let _ = flush stdout in 
		 let pb,(l,m),_ = get_boolean_encoding  None prefix' pb (l,m) in 
		 let pb,(l,m),_ = get_gathered_boolean_encoding None prefix' pb (l,m) in 
		 let pb,(l,m) = convert_contact prefix' (Some pb) (l,m) in 
	       	 match pb with None -> pb,(l,m)
		 |  Some pb -> 
		     let pb,(l,m),contact = get_high_res_contact_map prefix'  pb (l,m)  in 
		     let _ = set_packs pb in  
		 let (abstract_lens,rep1,rep2),sp,parsed_case,messages = Ite.itere pb m in
		 let rep = rep1,rep2 in 
		 let m,sol = 
		   RuleIdListMap.fold 
		     (fun r a sol ->
		       if a = None
		       then 
			 List.fold_left 
			   (fun (m,sol) r -> 
			     ("Rule "^(name_of_rule r)^" cannot be applied")::m,
			     RuleIdSet.add r sol)
			   sol r
		       else sol)
		     abstract_lens (m,RuleIdSet.empty) in
		 
	
		 let pb = 
		   {pb with Pb_sig.reachability_analysis = Some (StringMap.map A.summarize_abstract_expr (fst rep));
		     Pb_sig.unreachable_rules = Some sol;
		     bdd_sub_views = Some (StringMap.map A.compute_subviews rep1);
		     bdd_false = Some (StringMap.map A.compute_subviews sp);
		     contact_map = Some (snd rep) } in 
		 let l = chrono prefix "Reachability analysis" l in
		 Some pb,(l,m) )

       and refine_subviews prefix pb (l,m) =
         let prefix' = add_suffix prefix "refine_subviews" in 
	 match pb with
	   None -> None,(l,m)
	 | Some pb -> 
	     match pb.bdd_sub_views,pb.bdd_false,pb.contact_map 
	     with None,_,_ | _,_,None | _,None,_ -> 
	       let a,(l,m)=reachability_analysis prefix' (Some pb) (l,m) in
	       refine_subviews prefix' a (l,m)
	   | Some bdd,Some bdd2,Some contact ->
	            let _ = print_option prefix (Some stdout) "Abstract lenses computation\n" in

		    let rep1 = StringMap.map A.restore_subviews bdd  in 
		    let sp   = StringMap.map A.restore_subviews bdd2 in
		    let rep2 = contact in
		    let rep = rep1,rep2 in
		    let pack_val = Ite.print_subviews rep1 pb in
		    let pb = {pb with pack_value = Some pack_val} in
		    let pb,m = Ite.refine_pb rep pb sp m in
		    let l = chrono prefix "Absract lenses computation" l in
		    Some pb,(l,m) 
       and refine_views prefix pb (l,m) = 
	 let old_mem = !Config_complx.memoisation in 
	 let _ = Config_complx.memoisation:=false in 
	 let rep = try 
	   begin
	     let prefix' = add_suffix prefix "refine_views" in 
	     match pb  with
	       None -> None,(l,m)
	     | Some pb -> 
		 let _ = print_option prefix (Some stdout) "View computation\n" in
		 match pb.bdd_sub_views,pb.bdd_false,pb.contact_map 
		 with None,_,_ | _,_,None | _,None,_ -> 
		   let a,(l,m)=reachability_analysis prefix' (Some pb) (l,m) in
		   refine_views prefix a (l,m)
	       | Some bdd,Some bdd2,Some contact ->
		   let rep1 = StringMap.map A.restore_subviews bdd  in 
		   let sp   = StringMap.map A.restore_subviews bdd2 in
		   let rep2 = contact in
		   let rep = rep1,rep2 in
(*		   let pack_val = Ite.print_subviews rep1 pb in*)
		   let pb,m = Ite.refine_pb rep pb sp m in
		   try 
		     let conc,m = Ite.dump_concretization pb (rep,contact.relation_list)  m in 
		     
		     
		     let l,rep = 
		       if !Config_complx.dump_access or !Config_complx.dump_specie_map 
		       then 
			 (try (let rep = Ite.print_result rep pb in l,Some rep) 
			 with _ -> (l,None))
			   
		       else l,None
		     in 
		     let l = (chrono prefix "Views computation" l) in 
		     ( match rep 
		     with None -> Some pb,(l,m)
		     | Some (specie_map) -> Some {pb with 
						   Pb_sig.concretization = Some conc;
						   
						   specie_map = Some specie_map;
						 },(l,m))
		   with _ ->  let l = chrono prefix "View computation (aborted)" l in
		   Some pb,(l,m)
	   end
	     with _ -> 
	       let l = chrono prefix "View computation (aborted)" l in
	       pb,(l,m) in
	 (Config_complx.memoisation:=old_mem;
	 rep) 
       and dump_ckappa file mode prefix a c = 
	 match a with 
	   None -> a,c
	 | Some a' -> 
	     let cbng = 
	       if mode = Smashed 
	       then 
		 a'.gathered_intermediate_encoding 
	       else
		 a'.intermediate_encoding
	     in 
	     let _ = 
	       (match cbng with None -> ()
	       |  Some cbng -> Cbng_sig.cbng_dump file cbng)
	     in a,c
       and dump_boolean_encoding file mode prefix pb c =
		 match pb with None -> None,c
		| Some pb' ->
		  (  let bool = 
		      if mode = Smashed 
		      then pb'.gathered_boolean_encoding,pb'.gathered_intermediate_encoding,pb'.contact_map
		      else pb'.boolean_encoding,pb'.intermediate_encoding,pb'.contact_map
		    in 
		    (match bool  with 
		      Some a,Some b,Some con  -> (
			let _ = Pb_sig.dump_pb file a b con in 
		      (pb,c))
		    |  _ -> ((Some pb',c))))
       and dump_local_views file  = triv_step
      
       and dump_contact_map_txt res file prefix pb (l,m) =
	 if (not (is_contact_map res pb)) then pb,(l,m)
	 else 
	   begin
	     match pb with None -> None,(l,m)
	     |Some pb -> 
		 begin
		   let fic = file in 
		   if file = "" 
		   then  (Some pb,(l,m))
		   else 
		     if res = High 
		     then 
		       match pb.contact_map,!Config_complx.do_high_res_contact_map 
		       with 
			 None,false -> (Some pb,(l,m)) 
		       | _ -> 
			   let pb,(l,m),contact = get_high_res_contact_map prefix  pb (l,m) in 
			   let output = open_out fic in 
			   let _ = 
			     try ( 
			       let print s = Printf.fprintf output s in 
			       ((List.iter 
				   (fun (a,b) -> 
				     print "%s" (string_of_b (L((fst a,fst a,snd a),(fst b,fst b,snd b))));
				     print "\n")
				   contact.relation_list)
				  )) with Not_found -> () in 
			   let _ = close_out output in 
			      (Some pb,(chrono 
					  prefix  (
					"High resolution contact map (txt)")
					  l,m))
			     
			     
		     else 
		       match pb.intermediate_encoding,!Config_complx.do_low_res_contact_map
		       with None,false -> (Some pb,(l,m))
		       |  _ -> 
			   let pb,(l,m),contact = get_low_res_contact_map prefix  pb (l,m) in 
			   let output = open_out fic in 
			   let _ = 
			     try ( 
			       let print s = Printf.fprintf output s in 
			       match contact with 
				 None -> () 
			       |  Some contact -> 
				   ((String2Map.iter 
				       (fun a l ->
					 List.iter 
					   (fun b  -> 
					     
					     print "%s" (string_of_b (L((fst a,fst a,snd a),(fst b,fst b,snd b))));
					     print "\n") l)
				       contact
				       
				       ))) with Not_found -> () in 
			   let _ = close_out output in  
			   
			   (Some pb,(chrono prefix 
			       	       "Low resolution contact_map (txt)"
				       
				       l,m))
		 end
	   end
       and 
	   dump_contact_map_dot res file prefix pb (l,m) =
	 if not (is_contact_map res pb)
	 then pb,(l,m)
	 else 
	   begin 
	     match pb,file 
	     with 
	       None,_ | _ ,"" -> (pb,(l,m))
	     | Some a,_ -> 
		 if res = Low 
	     then 
		   match a.intermediate_encoding with None -> (pb,(l,m))
		   |  _ -> 
		       let _ = print_contact_map_in_dot res a in 
		       (pb,(chrono  prefix  "Low resolution contact (dot)" l,m))
		 else
		   match a.contact_map with None -> (pb,(l,m))
		   |  _ ->  let _ = print_contact_map_in_dot res a in 
		     (pb,(chrono  prefix  "High resolution contact (dot)" l,m))
	   end
       and 
	   dump_contact_map_ps res file prefix pb (l,m) =
	 if not (is_contact_map res pb) 
	 then pb,(l,m)
	 else 
	   begin
	     match pb,file,if res = High 
	     then (!Config_complx.output_high_res_contact_dot_file)
	     else (!Config_complx.output_low_res_contact_dot_file)
	     with None,_,_ | _,"",_ | _,_,"" -> (pb,(l,m))
       | Some a,b,c-> 
	   let _ = Sys.command ("dot -Tps "^c^" -o "^b) in 
	   (pb,(chrono prefix   
		  (if res = Low 
		  then "Low resolution contact_map (ps)"
		  else "High resolution contact map (ps)") l ,m))
	   end
       and 
           dump_contact_map_jpg res file prefix pb (l,m) = 
	 if not (is_contact_map res pb)
	 then pb,(l,m) 
	 else
	   begin
	     match pb,file,if res = High 
	     then (!Config_complx.output_high_res_contact_dot_file)
	     else (!Config_complx.output_low_res_contact_dot_file)
	     with None,_,_ | _,"",_ | _,_,"" -> (pb,(l,m))
	   | Some a,b,c-> 
	       let _ = Sys.command ("dot -Tjpg "^c^" -o "^b) in 
	       (pb,(chrono prefix  (if res = Low 
	       then "Low resolution contact_map (jpg)" 
	       else "High resolution contact map (jpg)") l ,m))
	   end
       and 
	   quarkification prefix pb' (l,m) = 
	 let prefix' = add_suffix prefix "quarkification" in
	 match pb' with 
	   None -> (pb',(l,m))
	 | Some pb -> 
	     let _ = print_option prefix (Some stdout) "Quark computation\n" in
	     let pb,(l,m),contact = get_best_res_contact_map prefix' pb (l,m) in 
	     let pb,(l,m),cpb = get_intermediate_encoding None prefix' pb (l,m) in
	   
	     let _ = flush stdout in 
	     Some {pb with intermediate_encoding = 
		    Some {cpb with 
			   cpb_rules = 
			   List.map (quarkify cpb contact) cpb.cpb_rules};quarks=true},(chrono prefix "Quark computation" l,m)
	       
       and
	   build_influence_map file file2 prefix pb' (l,m) =
	 let prefix' = add_suffix prefix "build_influence_map" in 
	 match pb' with 
	   None -> (pb',(l,m))
	 |Some pb ->
	     (match pb.wake_up_map,pb.inhibition_map 
	     with 
	       Some _,Some _ -> (pb',(l,m))
	     | _ ->
	
		 let _ = print_option prefix (Some stdout) "Influence map \n" in  
		 let pb,(l,m),cpb = get_intermediate_encoding None prefix' pb  (l,m) in 
		 let pb,(l,m) = 
		   if pb.quarks 
		   then Some pb,(l,m)
		   else quarkification prefix' (Some pb) (l,m) 
		 in 
		 
		 match pb with 
		   None -> (pb,(l,m))
		 | Some pb -> 
		     
		     let _ = flush stdout in 
		     let pb,(l,m),cpb = get_intermediate_encoding None prefix' pb  (l,m) in 
		     let rep = Influence_map.compute_influence_map cpb in 
		     let l = 
		       if file <> "" or file2 <> "" then 
			 let f = 
			   let map = 
			     List.fold_left 
			       (fun sol rc -> 
				 List.fold_left 
				   (fun sol (a,b,c) -> 
				     List.fold_left 
				       (fun sol id -> 
					 let kid = id.Pb_sig.r_simplx.Rule.id in
					 IntMap.add kid id sol)
				       sol a)
				   sol rc.cpb_guard)
			       IntMap.empty cpb.cpb_rules in
			   (fun x -> try (IntMap.find x map) with _ -> 
			     frozen_error "line 893" "" "build_influence_map"  (fun () -> raise Exit)) 
			 in
			 let _ = 
			   Tools2.log_in_file file 
			     (fun x -> 
			       let _ =  
				 IntMap.iter
				   (fun a b -> 
				     (IntSet.iter 
					(fun b -> Printf.fprintf x "%s->%s\n" 
					    (name_of_rule (f a)) 
					    (name_of_rule (f b))) b))
				   (fst rep),
				 IntMap.iter 
				   (fun a b -> 
				     (IntSet.iter 
					(fun b -> Printf.fprintf x "%s->%s\n" 
					    (name_of_rule (f a)) 
					    (name_of_rule (f b)))
					b)) (snd rep)
			       in ()) 
			 in 
			 let _ = 
			   Tools2.log_in_file file2
			     (fun x -> 
			       let set = 
				 IntMap.fold 
				   (fun a b c -> IntSet.union b (IntSet.add a c))
				   (fst rep)
				   (IntMap.fold 
				      (fun a b c -> IntSet.union b (IntSet.add a c))
				      (snd rep)
				      (IntSet.empty)) in 
			       let _ = Printf.fprintf x "DiGraph G {\n" in
			       let _ = 
				 IntSet.iter 
				   (fun r -> Printf.fprintf x "\"%s\" \n" (name_of_rule (f r)))
				   set in
			       let _ =  
				 IntMap.iter
				   (fun a b -> 
				     (IntSet.iter 
					(fun b -> Printf.fprintf x "\"%s\"->\"%s\" [color=green]\n" 
					    (name_of_rule (f a)) 
					    (name_of_rule (f b))) b))
				   (fst rep),
				 IntMap.iter 
				   (fun a b -> 
				     (IntSet.iter 
					(fun b -> Printf.fprintf x "\"%s\"-|\"%s\" [color=red] \n" 
					    (name_of_rule (f a)) 
					    (name_of_rule (f b)))
					b)) (snd rep)
			       in 
			       let _ = Printf.fprintf x "}\n" in 
			 ())
			 in 
			 let l=chrono prefix "Influence map" l 
			 in l 
		       else l
		     in 
			 (Some {pb 
			       with wake_up_map = Some (fst rep) ;
				 inhibition_map = Some (snd rep)},(l,m))) 

       and build_compression mode file1 file2 prefix pb (l,m) =  
	 let prefix' = add_suffix prefix "build_compression" in
	 let title = 
	   if mode = Full 
	   then "Compression(qualitative)"
	   else "Compression(quantitative)" in 
	 match pb,file1,file2   
	 with None,_,_   -> pb,(l,m)
       | Some pb',_,_ -> 
	   let _ = print_option prefix (Some stdout)  (title^"\n") in
	   let _ = flush stdout in
	   let pb',(l,m) = 
	     match mode with 
	       Full -> 
		 let pb,(l,m),_ = get_gathered_boolean_encoding None prefix' pb' (l,m) in 
		 pb,(l,m)
	     | Isolated -> 
		 let pb,(l,m),_ = get_boolean_encoding None prefix'  pb' (l,m) in 
		 pb,(l,m)
	     |  _ -> 
	   frozen_error "line 936" "Unknown compression mode" "build_compression" (fun () -> raise Exit) in 
	   (
	       let pb',(l,m),auto = get_auto prefix' pb' (l,m) in 
	       let auto x = 
		 try 
		     float_of_int (IntMap.find x  auto)
		 with 
		   Not_found -> 1. in 
	       let rep,m = Com.do_it file1 file2 mode auto pb' m in 
	       let l = chrono prefix title l in 
	       Some 
		 (match mode with Full -> {pb' with qualitative_compression = Some rep}
		 |  Isolated -> {pb' with quantitative_compression = Some rep}
		 | _ -> pb')
,(l,m))
       and build_pieces prefix pb (l,m) = 
	  match pb  with None -> None,(l,m)
	  |Some pb' -> 
	      match pb'.Pb_sig.concretization with 
		None -> None,(l,m) 
	      | Some conc -> 
		  let _ = trace_print "START ENUMERATION" in 
		  let rep3,m = get_gspecies conc pb'   m in
		  Some rep3,(l,m)

	       
	    
       and count_complexes prefix pb (l,m) = 
	 let prefix' = add_suffix prefix "count_complexes" in
	 match pb  with None -> pb,(l,m)
	 |Some pb' -> 
	     match pb'.Pb_sig.concretization with 
	       None -> pb,(l,m) 
	     | Some conc -> 
		 if Acyclicity.is_acyclic pb' then 
		   
		   let _ = trace_print "START NUMBERING" in 
		   let _ = print_option prefix (Some stdout) "Counting complexes\n" in
		   let rep3,(l,m) = build_pieces prefix' (Some pb') (l,m) in 
		   let n = 
		     match rep3 with None -> frozen_error "line 971" "Cannot build pieces" "count_complexe" (fun () -> raise Exit) | Some rep3 -> count rep3 in 
		   let l = chrono prefix "Counting complexes" l in
		   Some {pb' with n_complex = n},(l,m)
		 else
		   Some {pb' with n_complex = Some Unbounded},(l,m)

       and build_enumeration file prefix pb (l,m) = 
	 let prefix' = add_suffix prefix "build_enumeration" in 
	 match pb,file  with None,_ -> pb,(l,m)
       |Some pb',_ -> 
	   let output = open_file file in 
	   match pb'.Pb_sig.concretization with 
	     None -> pb,(l,m) 
	   | Some conc -> 
	       let print s = print_option prefix output s in 
	       let _ = trace_print "START ENUMERATION" in 
	       let _ = print_option prefix (Some stdout) "Enumerating complexes\n"  in 
	       if not (Acyclicity.is_acyclic pb') 
	       then 
	         let rep = "There are too many complexes so that we can enumerate them."
		   in 
		 let sol  = (print rep;print "\n";[[[rep]],0]) in 
		 let _ = close_file output in
		 let l = chrono prefix "Enumerating complexes" l in 
		 (Some {pb' with reachable_complexes = Some (Unbounded,sol,sol) ; 
		       n_complex = Some Unbounded}),(l,m)
	    
	       else
	       let rep3,(l,m) = build_pieces prefix' (Some pb') (l,m) in 
	       let rep3 = 
		 match rep3 with 
		   None -> frozen_error "line 1001" "cannot build pieces" "build_ennumeration" (fun () -> raise Exit) 
		 | Some rep3 -> rep3 in
	       let n,l  =   if Acyclicity.is_acyclic pb' then 
		 count rep3,chrono prefix "Complex counting" l  else Some Unbounded,l in 
	       let too,rep4,rep5,rep6,rep7,m  = 
		 match n with 
		   None | Some Unbounded -> true,[],[],Unbounded,Unbounded,m
		 | Some (Bounded k) when 
                     not (Big_int.is_int_big_int (Big_int.big_int_of_string k)) or  
		     (Big_int.int_of_big_int (Big_int.big_int_of_string k)) >(!Config_complx.complex_limit) -> true,[],[],Bounded k,Bounded "0",m
		 | Some (Bounded n)  -> 
		     let _ = trace_print "BUILD PIECES" in 
		     let rep4,rep5,rep6,rep7,m  = concretization rep3 m in 
		     false,rep4,rep5,
		     Bounded (string_of_int rep6),
		     Bounded (string_of_int rep7),m in
	 
	       let _ = trace_print "DUMP PIECES" in 
	       let rep = 
		 try (let print s = print_option prefix output s in 
		 let _ = if false then print_spec rep3 output in
		 let _ = print "ALL SPECIES : \n" in 
		 let _ = print "\n" in
		 let boundedlist  =  
		     if too then 
		       let rep = "There are too many complexes so that we can enumerate them."
		       in (print rep;print "\n";[[[rep]],0])
		     else 
		       List.fold_left 
			 (fun sol k -> 
			   let rep = print_complexe k rep3 output in                                       let _ = print "\n" in 
			   rep::sol) [] rep4  
		 in 
		 let _ = 
		   if rep7=Bounded "0" or (rep7 = Unbounded && rep6=Unbounded) 
		   then (print (match rep6 with Bounded n -> n
		   |  Unbounded -> "There is an unbounded number of");
		 	 print " species.") in 
		 let _ = print "\n\n" in
		 let unboundedlist = 
		   if rep7<>(Bounded "0") && rep7<>(Unbounded) then 
		     begin 
		       print "Unbounded Species:\n\n";
		       let list = 
			 List.fold_left 
			   (fun sol k -> 
			     let rep = print_complexe k rep3 output in
			     let _ = print "\n" in 
			     rep::sol)
			   [] rep5 in
		       let _ = print "\n\n" in 
		       list
		     end
		   else [] in 
		 let _ = print "\n" in 
		 match n with None -> None
		 | Some n -> Some (n,List.rev boundedlist,List.rev unboundedlist))
		 with Not_found -> None in 
	       let _ = close_file output in
	       let l = chrono prefix "Enumerating complexes" l in 
	       (Some {pb' with reachable_complexes = rep ; 
		      n_complex = Some rep6 }),(l,m)
	    
       and dump_session b prefix a c  = 
	   (
			 if b = "" then a,c
			 else 
			 let channel = open_out b in
			 let (c,m) = Xml.dump_session a channel c in 
			 let _ = close_out channel in 
			   a,(chrono prefix "dump_xml" c,m) )
       and 
	   dump_html_output = 
	 (fun b prefix a c -> 
	   if b = "" then a,c else
	   let channel = open_out b in 
	   let (c,m) = Html.dump_html a channel c in 
	   let _ = close_out channel in 
	   let bro = !Config_complx.html_browser in 
	   let _ = 
	     if bro <> "" 
	     then let _ = (Sys.command (bro^" "^b^" &") ) in () in 
	   a,(chrono prefix "dump_html" c,m))
       and refine_system_to_avoid_polymeres =
	 (fun file subsystem mode k kin_coef prefix pb (l,m) -> 
	   let prefix'= add_suffix prefix "Refine_system to avoid polymers"  in
	   let _ = print_option prefix (Some stdout) "Refine system to avoid polymers\n" in 
	   match pb with None -> [],None,(l,m)
	   | Some pb -> 
	       let pb,(l,m),cpb = 
		 get_intermediate_encoding None prefix'  pb (l,m) in 
	       let pb,(l,m) = 
		 reachability_analysis prefix' (Some pb) (l,m) in
	       match pb with None -> [],None,(l,m)
	       | Some pb -> 
	       let interface = cpb.cpb_interface in 
	       let subsystem,(l,m) = 
		 match subsystem with 
		   None -> None,(l,m)
		 | Some a -> 
		     match build_pb a prefix'  
		     with None -> None,(l,m)
		     | Some pb' -> 
		     let pb',(l,m),boolean_encoding  = 
		       get_boolean_encoding 
			 (Some interface) 
			 prefix'
			 pb' 
			 (l,m) in 
		     let boolean_encoding = 
		       match pb'.boolean_encoding 
		       with None -> error "line 1099" "Cannot build boolean encoding" "refine_system_to_avoid_polymeres" (raise Exit)
		       | Some boolean_encoding -> boolean_encoding in
		     let rule_system = boolean_encoding.system in 
		     (Some rule_system),(l,m) 
	       in  
	       let pb,(l,m),_ = 
		 get_boolean_encoding (Some interface) prefix' pb (l,m)  in  
	       let pb,rep,(l,m)  = 
		 Ref.avoid_polymere 
		   file 
		   subsystem 
		   k
		   kin_coef 
		   pb 
		   mode  
		   (l,m)  
	       in  
	       let pb,(l,m),auto = get_auto prefix' pb (l,m) in 
	       let auto x= 
		 try 
		   float_of_int (IntMap.find x auto) 
		 with 
		   Not_found -> 1. in 
	       let _ = Ref.dump (!Config_complx.output_without_polymere) auto pb (!Config_complx.cycle_depth) in 
	       let l = chrono prefix "System refinement" l in 
	       rep,(Some pb),(l,m))
       and get_auto prefix pb (l,m) = 
	 let prefix' = prefix in 
	 match 
	   pb.automorphisms 
	 with 
	   None -> 
	     let pb2,(l,m)  = count_automorphisms prefix' (Some pb) (l,m) in 
	     begin 
	       match pb2 with 
		 None -> error "line 1268" "cannot count automorphisms" "" (raise Exit)
	       | Some pb2 -> 
		   begin
		     match pb2.automorphisms 
		     with 
		       None -> error "line 1273" "Cannot count automorphisms" "" (raise Exit)
		     | Some a -> pb2,(l,m),a
		   end
	     end
	 | Some a -> pb,(l,m),a 
	   
     and template = 
	 (fun file0 file1 file2 file3 file4 file5 file6 file7 file8 file9 file10 file11 file12  file13 prefix pb (l,m) ->
	   let prefix' = add_suffix prefix "template" in 
	   let _ = print_option prefix (Some stdout) "Starting ODE generation\n" in
	   
	   let print_sb  sb pb (log:out_channel option) = 
	     let hash = Hashtbl.create 13  in
	     let nlink = ref 0 in
	     let _ = 
	       StringMap.fold 
		 (fun a b  bool ->
		   let _ = if bool then print_option empty_prefix  log "," in
		   let _ = 
		     A.print_reachable_states2 
		       string_txt 
		       (A.reachable_states_of_abstract_expr b)  
		       (fun x -> x) 
		       pb 
		       (Some "()")
		       (hash,nlink,(fun _ _ -> true))
		       log in 
		   true
		     )
		 sb false in
	     let _ = 
	       Hashtbl.iter 
		 (fun ((a,x),(a',x')) n -> 
		   let s = ","^a'^"("^x'^"!"^(string_of_int n)^")" in 
		   let _ = print_option empty_prefix  log s in 
		   ())
		 hash 
	     in () 
	   in  
	   let print_sb_latex x sb pb (log:out_channel option) = 
	     let hash = Hashtbl.create 13  in
	     let nlink = ref 0 in
	     let _ = 
	       StringMap.fold 
		 (fun a b  bool ->
		   let _ = if bool then print_option empty_prefix  log Latex.agent_sep  in
		   let _ = 
		     A.print_reachable_states2 
		       string_latex
		       (A.reachable_states_of_abstract_expr b)  
		       (fun x -> x) (*Latex.string_of_agent_name x)*) 
		       pb 
		       (Some "()")
		       (hash,nlink,x)
		       log in 
		   true
		     )
		 sb false in
	     let _ = 
	       Hashtbl.iter 
		 (fun ((a,x),(a',x')) n -> 
		   let s = Latex.agent_sep^(Latex.string_of_agent_name a')^"{"^(Latex.string_of_site_name x')^"{}{"^(string_of_int n)^"}}" in 
		   let _ = print_option empty_prefix  log s in 
		   ())
		 hash 
	     in () 
	   in  
	   let rep = pb in 
	   match rep  with None -> (rep,(l,m))
	   | Some rep' -> 
	       if not (is_views rep)
	       then (
		     let pb,log = reachability_analysis prefix' rep  (l,m) in
	             template 
		       file0 
		       file1 
		       file2 
		       file3 
		       file4 
		       file5 
		       file6 
		       file7 
		       file8 
		       file9 
		       file10 
		       file11 
		       file12 
		       file13 
		       prefix pb (l,m))
	       else (
		 match pb with 
		   None -> pb,(l,m) 
		 | Some a -> 
		     let pb,(l,m),boolean = get_boolean_encoding None prefix' a (l,m) in 
		     let pb,(l,m),auto = get_auto prefix' pb (l,m) in 
		     (match pb.bdd_sub_views with None -> Some pb,(l,m)
		     | Some sub ->
			 
			 let opt,(l,m)  = 
			   Ode_computation.compute_ode
			     file0 
			     file1 
			     file2
			     file3
			     file4 
			     file5
			     file6 
			     file7 
			     file8
			     file9 
			     file10 
			     file11
			     file12 
			     file13
			     {project=A.project;
			      export_ae = A.export_ae;
			      restore = A.restore_subviews;
			      b_of_var = A.K.E.V.b_of_var ;
			      var_of_b = A.K.E.V.var_of_b ;
				print_sb = print_sb;
				print_sb_latex = print_sb_latex;
			      fnd_of_bdd = A.fnd_of_bdd;
			      conj = A.conj ;
			      atom_pos = A.atom_pos ;
			      atom_neg = A.atom_neg ; 
			      expr_true = A.ae_true}

			     Ode_print_sig.MATLAB
			     prefix 
			     (Some stdout)
			     a boolean 
			     sub
			     auto 
			     (match !Config_complx.flat_ode 
			     with 
			       true -> Annotated_contact_map.Flat
			     | _ -> Annotated_contact_map.Compressed)

			     (l,m) in  
			 let nfrag = 
			   match opt with None -> None 
			   | Some(_,_,n) -> Some n 
			 in 
			 let l = chrono prefix "dumping fragments" l in 
			 Some {pb with nfrag = nfrag},(l,m))
		       ))
	   
       and 
	   marshallize  = 
	 (fun fic prefix pb (l,m) -> 
	   if fic = "" 
	   then pb,(l,m)
	   else 
	     let channel = open_out fic in 
	     let _ = Marshal.to_channel channel pb [] in 
	     let _ = close_out channel in 
	     let l = chrono prefix "marshalling" l in 
	     pb,(l,m))
       and 
	 unmarshallize  = 
	 (fun fic prefix (l,m) -> 
	   if fic = "" 
	   then None,(l,m)
	   else 
	     let channel = open_in fic in 
	     let pb = Marshal.from_channel channel in 
	     let _ = close_in channel in 
	     let l = chrono prefix "unmarshalling" l in 
	     pb,(l,m))
       and 
	   count_automorphisms = 
	 (fun prefix pb (l,m) -> 
	   match pb with 
	     None -> None,(l,m) 
	   | Some a -> 
	       let prefix'=add_suffix prefix "count automorphisms in lhs" in
	       let _ = print_option prefix (Some stdout) "count automorphism in lhs\n"  in 

	       let a,(l,m),boolean = get_boolean_encoding None prefix' a (l,m) in 
	       let rep,(l,m) = 
		 Count_isomorphism.count_isomorphism_in_rule_system  a boolean (l,m) in
	       let l = chrono prefix "count automorphisms in lhs" l in
	       Some rep,(l,m) )
       and 
	   compute_refinement_relation_closure  = 
	 (fun prefix pb (l,m) -> 
	   match pb with 
	     None -> None,(l,m)
	   | Some a -> 
	       let prefix' = add_suffix prefix "compute refinement relation closure" in 
	       let _ = print_option prefix (Some stdout) "compute refinement relation closure\n" in 
	       let a,(l,m),boolean = get_gathered_boolean_encoding None prefix' a (l,m) in 
	       
	       let rep = Refinements.compute_refinement boolean in
	       let l = chrono prefix "compute refinement relation closure" l in
	      
	       Some {a with refinement_relation_closure = Some rep},(l,m))
       and
	   compute_refinement_relation_maximal = 
	 (fun prefix pb (l,m) -> 
	   match pb 
	   with None -> None,(l,m)
	   |  Some a -> 
	       let prefix' = add_suffix prefix "compute maximal refinement relation" in
	       let _ = print_option prefix (Some stdout) "compute maximal refinement relation\n" in 
	       let pb,(l,m),rep =
		 match 
		   a.refinement_relation_closure 
		 with 
		   None -> 
		     begin
		       let pb,(l,m) = compute_refinement_relation_closure prefix' pb (l,m) in
		       match pb with 
			 None -> frozen_error "line 1295" "" "" (fun () -> raise Exit)
		       | Some a -> 
			   begin
			     match a.refinement_relation_closure with 
			       None -> frozen_error "line 1299" "" "" (fun () -> raise Exit)
			     | Some a -> pb,(l,m),a
			   end
		     end
		 | Some a -> pb,(l,m),a 
	       in
	       let rep = Refinements.compute_maximal_relation rep in
	       let l = chrono prefix "compute maximal refinement relation" l in
	       match pb with 
		 None -> None,(l,m)
	       | Some a -> 
		   Some {a with refinement_relation_maximale = Some rep},(l,m))
       and
	   export_refinement_relation = 
	 (fun prefix pb (l,m) -> 
	   match pb 
	   with None -> None,None,(l,m)
	   |  Some a -> 
	       let prefix' = add_suffix prefix "export refinement relation" in
	       let pb,(l,m),rep =
		 match 
		   a.refinement_relation_maximale 
		 with 
		   None -> 
		     begin
		       let pb,(l,m) = 
			 compute_refinement_relation_maximal
			   prefix' pb (l,m) 
		       in
		       match pb with None -> frozen_error "line 1328" "" "" (fun () -> raise Exit)
		       | Some a -> 
			   begin
			     match a.refinement_relation_maximale with 
			       None -> frozen_error "line 1332" "" "" (fun () -> raise Exit)
			     | Some a -> pb,(l,m),a
			   end
		     end
		 | Some a -> pb,(l,m),a
	       in
	       let rules = 
		 match a.simplx_encoding with 
		   None -> frozen_error "line 1340" "" "" (fun () -> raise Exit)
		 | Some (a,_) -> a in 
	       let rep = 
		 List.fold_left 
		   (fun a b -> 
		     IntMap.fold 
		       (fun b i map -> 
			 IntMap.add b i map)
		       b a)
		   IntMap.empty rep in 
	       let rule_map = 
		 List.fold_left 
		   (fun map a -> 
		     let id = a.Rule.id in
		     IntMap.add id a map)
		   IntMap.empty rules 
	       in 
	       let rules = 
		 List.map 
		   (fun r -> 
		     let id = r.Rule.id in 
		     let image = 
		       try 
			 IntMap.find id rep
		       with 
			 Not_found -> error_frozen "line 1365" "" "" (fun () -> raise Exit) in
		     let targets = 
		       IntSet.fold 
			 (fun a l -> 
			   (try 
			     IntMap.find a rule_map
			   with 
			     Not_found -> error_frozen "line 1372" "" "" (fun () -> raise Exit))::l)
			 image [] in
		     {r with Rule.abstraction = Some targets})
		   rules in
	        let l = chrono prefix "export maximal refinement relation" l in
	      
	       (Some rules),pb,(l,m) )
       and
	   export_automorphism_number = 
	 (fun prefix pb (l,m) -> 
	   match pb 
	   with None -> None,None,(l,m)
	   |  Some a -> 
	       let prefix' = add_suffix prefix "export refinement relation" in
	       let pb,(l,m),rep =
		 match 
		   a.automorphisms 
		 with 
		   None -> 
		     begin
		       let pb,(l,m) = 
			 count_automorphisms 
			   prefix' pb (l,m) 
		       in
		       match pb with None -> frozen_error "line 1411" "" "" (fun () -> raise Exit)
		       | Some a -> 
			   begin
			     match a.automorphisms with 
			       None -> frozen_error "line 1332" "" "" (fun () -> raise Exit)
			     | Some a -> pb,(l,m),a
			   end
		     end
		 | Some a -> pb,(l,m),a
	       in
	       let rules = 
		 match a.simplx_encoding with 
		   None -> frozen_error "line 1340" "" "" (fun () -> raise Exit)
		 | Some (a,_) -> a in 
	       let rules = 
		 List.map 
		   (fun r -> 
		     let id = r.Rule.id in 
		     let image = 
		       try 
			 (IntMap.find id rep)
		       with 
			 Not_found -> error_frozen "line 1365" "" "" (fun () -> raise Exit) in
		     {r with Rule.automorphisms = Some image})
		   rules in
	        let l = chrono prefix "export automorphims number" l in 
	      
	       (Some rules),pb,(l,m))
       and
           export_refinement_relation_and_automorphisms  = 
	 (fun prefix pb (l,m) -> 
	   match pb 
	   with None -> None,None,(l,m)
	   |  Some a -> 
	       let prefix' = add_suffix prefix "export refinement relation" in
	       let pb,(l,m),rep =
		 match 
		   a.refinement_relation_maximale 
		 with 
		   None -> 
		     begin
		       let pb,(l,m) = 
			 compute_refinement_relation_maximal
			   prefix' pb (l,m) 
		       in
		       match pb with None -> frozen_error "line 1328" "" "" (fun () -> raise Exit)
		       | Some a -> 
			   begin
			     match a.refinement_relation_maximale with 
			       None -> frozen_error "line 1332" "" "" (fun () -> raise Exit)
			     | Some a -> pb,(l,m),a
			   end
		     end
		 | Some a -> pb,(l,m),a
	       in
	       let pb,(l,m),rep2 =
		 match 
		   a.automorphisms 
		 with 
		   None -> 
		     begin
		       let pb,(l,m) = 
			 count_automorphisms 
			   prefix' pb (l,m) 
		       in
		       match pb with None -> frozen_error "line 1411" "" "" (fun () -> raise Exit)
		       | Some a -> 
			   begin
			     match a.automorphisms with 
			       None -> frozen_error "line 1332" "" "" (fun () -> raise Exit)
			     | Some a -> pb,(l,m),a
			   end
		     end
		 | Some a -> pb,(l,m),a
	       in
	       let rules = 
		 match a.simplx_encoding with 
		   None -> frozen_error "line 1340" "" "" (fun () -> raise Exit)
		 | Some (a,_) -> a in 
	       let rep = 
		 List.fold_left 
		   (fun a b -> 
		     IntMap.fold 
		       (fun b i map -> 
			 IntMap.add b i map)
		       b a)
		   IntMap.empty rep in 
	       let rule_map = 
		 List.fold_left 
		   (fun map a -> 
		     let id = a.Rule.id in
		     IntMap.add id a map)
		   IntMap.empty rules 
	       in 
	       let rules = 
		 List.map 
		   (fun r -> 
		     let id = r.Rule.id in 
		     let image = 
		       try 
			 IntMap.find id rep
		       with 
			 Not_found -> error_frozen "line 1365" "" "" (fun () -> raise Exit) in
		     let targets = 
		       IntSet.fold 
			 (fun a l -> 
			   (try 
			     IntMap.find a rule_map
			   with 
			     Not_found -> error_frozen "line 1372" "" "" (fun () -> raise Exit))::l)
			 image [] in
		     let image2 = 
		       try 
			 Some (IntMap.find id rep2) 
		       with 
			 Not_found -> None in 
		     {r with Rule.abstraction = Some targets;
                             Rule.automorphisms = image2 })
		   rules in
	        let l = chrono prefix "export maximal refinement relation" l in
	      
	       (Some rules),pb,(l,m) )
        	   
       and 
	   compute_refinement_relation_dag =
	  (fun prefix pb (l,m) -> 
	   match pb 
	   with None -> None,(l,m)
	   |  Some a -> 
	       let prefix' = add_suffix prefix "compute dag-like refinement relation" in
	       let _ = print_option prefix (Some stdout) "compute dag-like refinement relation\n" in 
	       let pb,(l,m),rep =
		 match 
		   a.refinement_relation_closure 
		 with 
		   None -> 
		     begin
		       let pb,(l,m) = compute_refinement_relation_closure prefix' pb (l,m) in
		       match pb with 
			 None -> frozen_error "" "" "" (fun () -> raise Exit)
		       | Some a -> 
			   begin
			     match a.refinement_relation_closure with 
			       None -> frozen_error "" "" "" (fun () -> raise Exit)
			     | Some a -> pb,(l,m),a
			   end
		     end
		 | Some a -> pb,(l,m),a 
	       in
	       let rep = Refinements.compute_dag_relation rep in
	       let l = chrono prefix "compute dag-like refinement relation" l in
	       match pb with 
		 None -> None,(l,m)
	       | Some a -> 
		   Some {a with refinement_relation_dag = Some rep},(l,m))
       and dump_refinement_relation (get,compute,string) = 
	 (fun file file2 prefix pb (l,m) -> 
	   let _  = add_suffix prefix string in 
	   if file="" 
	   then pb,(l,m)
	   else
	     begin
	       match pb with None -> pb,(l,m)
	       | Some a -> 
		   let rules = 
		     match a.simplx_encoding with 
		       None -> frozen_error "line 1422" "" "" (fun () -> raise Exit)
		     | Some (a,_) -> a in 
		   match get a with 
		     Some a -> 
		       let _ = 
			 Refinements.dump_refinement file file2 rules a  in
		       let l = chrono prefix string l 
		       in pb,(l,m) 
		   | None -> pb,(l,m)
	     end)
       and dump_maximal_refinement_relation x = 
	 dump_refinement_relation 
	   ((fun a -> a.refinement_relation_maximale),
	    compute_refinement_relation_maximal,
	    "dump_maximal_refinement_relation") x
       and dump_dag_refinement_relation x = 
	 dump_refinement_relation 
	   ((fun a -> a.refinement_relation_dag),
	    compute_refinement_relation_dag,
	    "dump_dag_refinement_relation") x
       and dump_latex_dictionary x prefix pb log = 
	 let _ = Latex.dump_dictionary x in
	 pb,log 
       and print_error prefix = 
	 match !Error_handler_common.error_list 
	 with 
	   [] -> ()
	 | list -> 
	     let _ = Printf.fprintf stderr "Errors:" in
	     let _ = Printf.fprintf stderr "\n" in
	     let _ = 
	       List.iter 
		 (fun error -> Printf.fprintf stderr "%s\n" (Error_handler.string_of_error error))
		 list 
	     in (flush stderr)
       and
	   dump_latex_rule_system file prefix pb log = 
	 match pb with 
	   None -> pb,log
	 | Some a -> 
	     let pb',log,_ = get_boolean_encoding None prefix a log in 
	     begin
	       let _ = Latex.dump file pb'
		     (A.K.E.V.var_of_b,
		      A.K.E.V.b_of_var,
		      A.K.E.V.varset_add,
		      A.K.E.V.varset_empty,
		      A.K.E.V.fold_vars,
		      A.K.build_kleenean_rule_system,
		      (fun is_access f  g set  ss print_any sigma sigma2 ret log -> 
			A.K.print_kleenean_system 
			  string_latex 
			  is_access 
			  f 
			  g 
			  set 
			  ss
			  print_any 
			  sigma 
			  sigma2 
			  ret 
			  None 
			  log))in 
		 (Some pb'),log
	     end
       and
	   dump_latex_version file prefix pb log = 
	 let _ = Latex.dump_version file in 
	 pb,log 
       and
	   dump_latex_stat file prefix pb log = 
	 let _ = Latex.dump_stat file (!Config_complx.latex_session_title) ((string_of_float (full_time (Unix.times ())))^" s.") in 
	 pb,log 
       and 
	   dump_latex_species_number file prefix pb log = 
	 let _ = Latex.dump_nspecies file pb in 
	 pb,log
       and 
	   dump_latex_fragments_number file prefix pb log = 
	 let _ = Latex.dump_nfrag file pb in 
	 pb,log
       and 
	   dump_latex_rules_number file prefix pb log = 
	 let _ = Latex.dump_nrule file pb in 
	 pb,log
       in
       {
       
       reset=handle_errors_step (Some "Complx") (Some "reset") reset;
       save_options = handle_errors_step (Some "Complx") (Some "save_options") save_options;
       print_footpage = (fun a -> handle_errors_homo (Some "Complx") (Some "print_foot_page") (print_footpage a));
       print_headpage = (fun a -> handle_errors_homo (Some "Complx") (Some "print_headpage") (print_headpage a));
       dump_version = (fun a -> handle_errors_homo (Some "Complx") (Some "dump_version") (dump_version a));
       empty_channel = empty_channel;
       log_time = (fun a b -> handle_errors_homo (Some "Complx") (Some "log_time") (log_time a b));
       add_message = (fun a b -> handle_errors_homo (Some "Complx") (Some "add_message") (add_message a b));
       print_channel  = (fun a -> handle_errors_main (Some "Complx") (Some "print_channel") (print_channel a));
       build_pb = handle_errors (Some "Complx") (Some "build_pb") build_pb;
       parse_file = 
       (fun a b c -> 
	 (handle_errors_return 
	    (Some "Complx") 
	    (Some "parse_file") 
	    (parse_file a b) 
	    c
	    (None,c)) 
	   )  ;
       translate = (fun x -> handle_errors_step (Some "Complx") (Some "translate") (translate x));
       compile = (fun x a -> handle_errors_step  (Some "Complx") (Some "compile")(compile x a));
       build_contact = (fun a -> handle_errors_step (Some "Complx") (Some "build_contact") (build_contact a));
       parse_line_by_line = (fun a -> handle_errors_step (Some "Complx") (Some "parse_line_by_line") (parse_line_by_line a));
       reachability_analysis = handle_errors_step (Some "Complx") (Some "reachability_analysis") reachability_analysis;
       dump_ckappa = (fun a b -> handle_errors_step (Some "Complx")  (Some "dump_ckappa") (dump_ckappa a b));
       dump_boolean_encoding = (fun a b -> handle_errors_step (Some "Complx") (Some "dump_boolean_encoding") (dump_boolean_encoding a b));
       dump_local_views=(fun a -> handle_errors_step (Some "Complx") (Some "dump_local_views") (dump_local_views a));
       dump_contact_map_txt= (fun a b -> handle_errors_step (Some "Complx") (Some "dump_contact_map_txt") (dump_contact_map_txt a b));
       dump_contact_map_dot= (fun a b -> handle_errors_step (Some "Complx") (Some "dump_contact_map_dot") (dump_contact_map_dot a b));
       dump_contact_map_ps = (fun a b -> handle_errors_step (Some "Complx") (Some "dump_contact_map_ps") (dump_contact_map_ps a b));
       dump_contact_map_jpg = (fun a b -> handle_errors_step (Some "Complx") (Some "dump_contact_map_jpg") (dump_contact_map_jpg a b));
       build_influence_map= (fun a b -> handle_errors_step (Some "Complx") (Some "build_influence_map") (build_influence_map a b));
       build_compression = (fun a b c -> handle_errors_step (Some "Complx") (Some "build_compression") (build_compression a b c));
       build_enumeration = (fun a -> handle_errors_step (Some "Complx") (Some "build_enumeration") (build_enumeration a));
       dump_packs_constraints  =(fun file -> triv_step);
       dump_session = (fun a -> handle_errors_step (Some "Complx") (Some "dump_session") (dump_session a));
       dump_html_output = (fun a -> handle_errors_step (Some "Complx") (Some "dump_html_output") (dump_html_output a));
       marshallize = (fun a -> handle_errors_step (Some "Complx") (Some "marshallize") (marshallize a));
       unmarshallize =(fun a  -> handle_errors_none (Some "Complx") (Some "unmarshallize") (unmarshallize a));
       count_complexes = handle_errors_step (Some "Complx") (Some "count_complexes") count_complexes;
       quarkification = handle_errors_step (Some "Complx") (Some "quarkification") quarkification;
       refine_subviews = handle_errors_step (Some "Complx") (Some "refine_subviews") refine_subviews;
       refine_views = handle_errors_step (Some "Complx") (Some "refine_views") refine_views ; 
       good_vertice = (fun a b c -> 
	 try good_vertice a b c 
	 with 
	   Exception _ -> None,c);
       template = (fun a b c d e f g h i j k l m n -> handle_errors_step (Some "Complx") (Some "template") (template a b c d e f g h i j k l m n));
       find_potential_cycles = (fun a -> handle_errors_step (Some "Complx") (Some "find_potential_cycles") (find_potential_cycles a));
       dump_potential_cycles = (fun a -> handle_errors_step (Some "Complx") (Some "dump_potential_cycles") (dump_potential_cycles a)) ;
       find_connected_components = (fun a -> handle_errors_step (Some "Complx") (Some "find_connected_components") (find_connected_components a));
       refine_system_to_avoid_polymers = 
       (fun a b c d e  -> handle_errors_def (Some "Complx") (Some "refine_system_to_avoid_polymers") (refine_system_to_avoid_polymeres a b c d e ) []);
       build_drawers = 
        handle_errors_step (Some "Complx") (Some "build_drawers") build_drawers ;
       count_automorphisms = 
        handle_errors_step (Some "Complx") (Some "count_automorphisms") count_automorphisms ; 
       compute_refinement_relation_maximal = 
       handle_errors_step (Some "Complx") (Some "compute_refinement_relation_maximal") compute_refinement_relation_maximal ; 
       compute_refinement_relation_dag = 
       handle_errors_step (Some "Complx") (Some "compute_refinement_relation_DAG") compute_refinement_relation_dag ;
       compute_refinement_relation_closure = 
       handle_errors_step (Some "Complx") (Some "compute_refinement_relation_closure") compute_refinement_relation_closure ; 
       
       dump_maximal_refinement_relation = 
       (fun file file2 -> handle_errors_step (Some "Complx") (Some "output_maximal_refinement_relation") (dump_maximal_refinement_relation file file2)) ;
       
       dump_latex_dictionary = 
       (fun file -> handle_errors_step (Some "Complx") (Some "latex dictionary") (dump_latex_dictionary file));
       
       dump_dag_refinement_relation = 
       (fun file file2 -> handle_errors_step (Some "Complx") (Some "output_dag_refinement_relation") (dump_dag_refinement_relation file file2)) ;
       
       export_refinement_relation_maximal = 
       handle_errors_def (Some "Complx") (Some "compute_refinement_relation") export_refinement_relation None ;

       export_automorphism_number = 
       handle_errors_def (Some "Complx") (Some "compute_automorphism_numbers")  export_automorphism_number  None ;

       export_refinement_relation_maximal_and_automorphism_number = 
              handle_errors_def (Some "Complx") (Some "compute_both_refinement_relation_and_automorphism_numbers")  export_refinement_relation_and_automorphisms  None ;
       print_errors = print_error ;
       dump_latex_rule_system = 
       (fun file -> handle_errors_step (Some "Complx") (Some "dump_latex_rule_system") (dump_latex_rule_system file));
       dump_latex_version =  (fun file -> handle_errors_step (Some "Complx") (Some "dump_latex_version") (dump_latex_version file));
        dump_latex_stat =  (fun file -> handle_errors_step (Some "Complx") (Some "dump_latex_stat") (dump_latex_stat file));
        dump_latex_fragments_number =  (fun file -> handle_errors_step (Some "Complx") (Some "dump_latex_fragment_number") (dump_latex_fragments_number file));
        dump_latex_species_number =  (fun file -> handle_errors_step (Some "Complx") (Some "dump_latex_species_number") (dump_latex_species_number file));
         dump_latex_rules_number =  (fun file -> handle_errors_step (Some "Complx") (Some "dump_latex_rules_number") (dump_latex_rules_number file));
     } 
	 
	 
   end
)
	 


module ExprB=Expr(Var(RuleBool))
module ExprK=Kleenean_expr(ExprB)(Id)
module BDD = BddBool(ExprK)
module ABDD = Part(BDD)(ZERO)  (* SBDD === BDD mais en plus lent !!! *)

module MTTTF = Pipeline(Part(BDD)(QSitePhosLink))
module MFTTF = Pipeline(Part(BDD)(QPhosLink))
module MFFTF = Pipeline(Part(BDD)(QLink))
module MTFTF = Pipeline(Part(BDD)(QSiteLink))
module MTTFF = Pipeline(Part(BDD)(PSitePhos))
module MFTFF = Pipeline(Part(BDD)(PPhos))
module MFFFF = Pipeline(BDD)
module MTFFF = Pipeline(Part(BDD)(PSite))
module MTTTT = Pipeline(Part(BDD)(RSitePhosLink))
module MFTTT = Pipeline(Part(BDD)(RPhosLink))
module MFFTT = Pipeline(Part(BDD)(RLink))
module MTFTT = Pipeline(Part(BDD)(RSiteLink))
module MTTFT = Pipeline(Part(BDD)(RSitePhos))
module MFTFT = Pipeline(Part(BDD)(RPhos))
module MFFFT = Pipeline(Part(BDD)(APhos))
module MTFFT = Pipeline(Part(BDD)(RPSite))

module ATTTF = Pipeline(Part(BDD)(Auto(QSitePhosLink)))
module AFTTF = Pipeline(Part(BDD)(Auto(QPhosLink)))
module AFFTF = Pipeline(Part(BDD)(Auto(QLink)))
module ATFTF = Pipeline(Part(BDD)(Auto(QSiteLink)))
module ATTFF = Pipeline(Part(BDD)(Auto(PSitePhos)))
module AFTFF = Pipeline(Part(BDD)(Auto(PPhos)))
module AFFFF = Pipeline(Part(BDD)(PAuto))
module ATFFF = Pipeline(Part(BDD)(Auto(PSite)))
module ATTTT = Pipeline(Part(BDD)(Auto(RSitePhosLink)))
module AFTTT = Pipeline(Part(BDD)(Auto(RPhosLink)))
module AFFTT = Pipeline(Part(BDD)(Auto(RLink)))
module ATFTT = Pipeline(Part(BDD)(Auto(RSiteLink)))
module ATTFT = Pipeline(Part(BDD)(Auto(RSitePhos)))
module AFTFT = Pipeline(Part(BDD)(Auto(RPhos)))
module AFFFT = Pipeline(Part(BDD)(Auto(APhos)))
module ATFFT = Pipeline(Part(BDD)(Auto(RPSite)))

let log = () 
let methods () =
  match 
    !Config_complx.auto_packs,
    !Config_complx.site_abstraction,
    !Config_complx.phospho_abstraction,
    !Config_complx.ignore_linkage,
    !Config_complx.ignore_phospho with 
    true,true,true,true,true -> ATTTT.build log 
  | true,false,true,true,true -> AFTTT.build log  
  | true,false,false,true,true -> AFFTT.build log 
  | true,true,false,true,true -> ATFTT.build log 
  | true,true,true,false,true -> ATTFT.build log 
  | true,false,true,false,true -> AFTFT.build log 
  | true,false,false,false,true -> AFFFT.build log  
  | true,true,false,false,true -> ATFFT.build log 
  | true,true,true,true,false -> ATTTF.build log  
  | true,false,true,true,false -> AFTTF.build log 
  | true,false,false,true,false -> AFFTF.build log 
  | true,true,false,true,false -> ATFTF.build log 
  | true,true,true,false,false -> ATTFF.build log 
  | true,false,true,false,false -> AFTFF.build log 
  | true,false,false,false,false -> AFFFF.build log 
  | true,true,false,false,false -> ATFFF.build log 
  | false,true,true,true,true -> MTTTT.build log  
  | false,false,true,true,true -> MFTTT.build log 
  | false,false,false,true,true -> MFFTT.build log 
  | false,true,false,true,true -> MTFTT.build log 
  | false,true,true,false,true -> MTTFT.build log 
  | false,false,true,false,true -> MFTFT.build log 
  | false,false,false,false,true -> MFFFT.build log  
  | false,true,false,false,true -> MTFFT.build log 
  | false,true,true,true,false -> MTTTF.build log  
  | false,false,true,true,false -> MFTTF.build log 
  | false,false,false,true,false -> MFFTF.build log 
  | false,true,false,true,false -> MTFTF.build log 
  | false,true,true,false,false -> MTTFF.build log 
  | false,false,true,false,false -> MFTFF.build log 
  | false,false,false,false,false -> MFFFF.build log 
  | false,true,false,false,false -> MTFFF.build log 



