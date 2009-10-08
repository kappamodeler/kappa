(* 10/05/2008 *)
(* Static analysis of BNG systems*)
(* Jerome Feret pour PlectiX *)
(* Configuration *)
(* config.ml *)

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

let version = "4.16.."^(string_of_int Git_commit_info.git_commit_tag)
let date = "2009.07.20"
let date_commit = Git_commit_info.git_commit_date 
let input_marshalling = ref "" 
let input_file = ref [""] 
let input_focus_on = ref ""
let refine_fic = ref ""
let key = ref "0000000000000000" 

(*External applications *)
let html_browser = ref Config.browser_command

(* Trace *)
let dump_chrono = ref true 
let dump_version = ref false 
let efficient = ref true 
let trace=ref false (* debug *)
let unsafe_mode=ref false (*debug *)
let trace_rule_iteration = ref true (* reachability: print # of rule changing invariant)*)
let trace_iteration_number = ref true (* reachability: print each new iteration *)

(* Dump answer *)
    (*reachability analysis *)

let dump_access = ref true
let dump_specie_map = ref true
let find_potential_cycles = ref false
let find_connected_components = ref false 
let dump_potential_cycles = ref false
    (*rule compression*)

type compression_mode = Full | With_cynetic | Isolated 

let max_lens_size = ref 5
let compression_mode = ref Full
let trace_reachable_states = ref false  (* compression: print domain of the group of rules*)
let trace_concrete_rules = ref true    (* compression: print concrete rules *)
let trace_abstract_rules = ref true   (* print compressed rules *)

let complex_limit = ref 100000
let do_ODE = ref false
let integrate_ODE = ref false 
let do_XML_session = ref true
let do_HTML_session = ref true
let do_marshalling = ref true 
let do_low_res_contact_map = ref true
let do_high_res_contact_map = ref true 
let do_local_views = ref false 
let do_enumeration = ref false
let do_influence = ref true 
let do_qualitative_compression = ref true
let do_quantitative_compression = ref true 
    (*all species *)
let do_something () = 
   not (!input_file = [""] or !input_file = [])
   or !input_marshalling <> ""  
let do_dump_latex = ref true 

let dump_all_complexs = ref true
let sort_complexes = ref true
let count_complexes = ref false
let log_each = ref 0

let wake_up = ref true
let inhibition = ref false
(* Data structures *)
    (* hash *)

let ode_points = ref 1000 
let ode_init_time = ref 0.
let ode_final_time = ref 1.
let ode_init_step = ref 0.000001
let memory_limit = ref 0 
let hashinit= ref 20000
let hash_cons = ref true
let memoisation = ref true
let local_memoisation = ref false 
let flat_ode = ref false 


let cycle_depth = ref 10 
let force_cycle = ref false 
let output_without_polymere = ref "" 
let only_closing_rules = ref false 
let kinetic_amplifier = ref 1. 

let display_unreachable_rules_in_contact_map = ref false 


(* Analysis parameters *)
    (* reachability analysis *)


         (* enforce structural invariant *)



let refine_after_instanciation = ref false
let refine_after_guard = ref false 
let refine_after_control = ref false
let refine_during_export = ref false 

         (* domain abstraction *)

let site_abstraction = ref false    (*true -> ignore dependences between distinct sites*)
let phospho_abstraction = ref false (*true -> ignore dependences between phosphorilation and other predicates *)
let ignore_linkage = ref false      (*true -> drop precise information about linkage *)
let ignore_phospho = ref false       (*true -> drop any information about phosphorilation *)
let auto_packs = ref true 
(*let packs = ref ((StringMap.empty):string list list StringMap.t)*)
    (* rule compression *)

let ignore_dep = ref false  (*true -> peut abstraire partiellement un site *)
let enforce_structural_invariants = ref true (*use the structural properties of boolean encoding to restore abstracted constraints*)
    
let duplicate_rules_when_sym = ref true (*true*)
let duplicate_rules_when_cycles = ref true (* true *)(* not_implemented yet *)
let dump_qualitative_compression = ref ""
let dump_quantitative_compression = ref "" 

(* internal bahaviour *)

let build_relationships_among_species = ref false




(* pretty printing *)


let empty_interface = ref "(Any)" (*to be printed when we have no information at all about the sites of an agent *)
let skip_a_specie   = ref ""      (*to be printed when an agent can be fully abstracted *)

let ode_output_mode = ref "OCTAVE" 
let ode_memoization_level = ref 1
let output_latex_rule_system = ref ""
let output_latex_sty = ref ""
let output_ODE_xml = ref "" 
let output_ODE_latex = ref ""
let output_ODE_obs_head = ref ""
let output_ODE_data = ref ""
let output_ODE_obs_latex = ref ""
let output_ODE_covering = ref ""
let output_ODE_covering_latex = ref "" 
let output_ODE_contact = ref ""
let output_ODE_mathematica = ref ""
let output_ODE_octave_size = ref "" 
let output_ODE_octave = ref "" 
let output_ODE_octave_aux = ref ""
let output_ODE_octave_jacobian = ref "" 
let output_ODE_octave_activity = ref ""
let output_ODE_octave_obs = ref "" 
let output_ODE_octave_init = ref ""
let output_ODE_alphabet = ref "" 
let output_ODE_obs = ref ""
let output_ODE_gplot = ref "" 
let output_ODE_png = ref "" 
let output_ODE_data = ref "" 
let output_ODE_script = ref "" 
let output_influence_map_dot_file = ref "" 
let output_influence_map_jpg_file = ref "" 
let output_marshalling = ref ""
let output_influence_map_txt_file = ref ""
let output_low_res_contact_dot_file = ref "" 
let output_low_res_contact_ps_file = ref "" 
let output_low_res_contact_jpg_file = ref ""
let output_low_res_contact_txt_file = ref ""
let output_high_res_contact_dot_file = ref "" 
let output_high_res_contact_ps_file = ref "" 
let output_high_res_contact_jpg_file = ref ""
let output_high_res_contact_txt_file = ref ""
let correct_contact_map () = 
  let _ = 
    if !output_low_res_contact_dot_file = ""
	&& 
      (!output_low_res_contact_ps_file <> "" or 
       !output_low_res_contact_jpg_file <> "")
    then output_low_res_contact_dot_file := ".low_res_tmp.dot" in
  let _ = 
    if !output_high_res_contact_dot_file = ""
	&& 
      (!output_high_res_contact_ps_file <> "" or 
       !output_high_res_contact_jpg_file <> "")
    then output_high_res_contact_dot_file := ".high_res_tmp.dot" in
  ()
let output_latex_version = ref ""
let output_latex_stat = ref ""
let output_latex_fragment = ref ""
let output_latex_rules = ref ""
let output_latex_species = ref ""
let output_dag_ref_dot = ref ""
let output_dag_ref_jpg = ref ""
let output_maximal_ref_dot = ref ""
let output_maximal_ref_jpg = ref ""
let output_pack_value_file  = ref ""
let output_gathered_cbng = ref ""
let output_gathered_boolean_encoding = ref "" 
let output_reachable_complexes = ref ""
let output_cbng = ref "" 
let output_boolean_encoding = ref "" 
let output_xml = ref "" 
let output_html = ref "" 
let output_specie_map = ref ""
let output_pretty_qualitative_compression = ref "" 
let output_pretty_quantitative_compression = ref "" 
let string_of_site (a,b) = a^(if b = "" then "" else ("."^b))
let site_separator = ref ","
let solution_separator = ref ","
let solution_complex = ref "."
let created_species = ref ""
let mark_symbol = ref "~"
let latex_session_title = ref "Session"
let bound_symbol = ref "!"

let free = ref ""                (* to be printed when a site is free *) 
let bound_or_not = ref "?"       (* to be printed when we do not know when a site is bounded, or not *)
let bound_undefined = ref ""     (* to be printed when a site is not a binding site *)
let bound_abstracted = ref ""    (* to be printed when the binding of a site is not considered by the abstract domain *)
let bound_of_number n = (string_of_int n,"") (* how to print a numbered link *)
let bound_of_known s = (*(!bound_symbol)^("_")*) !bound_symbol^(string_of_site s) (* to be printed when we know to which a site is bound *)
let bound_not_site s = !bound_symbol^"not("^(string_of_site s)^")" (* to be printed when we know that a site cannot be bound to something compatible with s*)
let bound_to_unknown k = (!bound_symbol)^("_") (*!bound_symbol^(string_of_site (bound_of_number k))  (* to be printed when a site is bound, but we have no information about to which it is bound to *)*)

let unmark = ref ""               (* to be printed when a site is not marked*)
let not_mark a = !mark_symbol^"not("^a^")" (* to be print when we know that a site is not marked with the mark a*)
let mark_of x = !mark_symbol^x  (* to be printed when a site is marked with the mark a*) 
let mark_abstracted = ref ""       (* to be printed when the abstract domain abstract away the mark of a site *)
let mark_undefined =  ref ""       (* to be printed when a site cannot be marked *) 
let mark_unknown = ref (!mark_symbol^"?") (*to be printed when we have n information about the mark of a site *)
let marked_or_not = ref ""             (*to be printed when we de no know wether a site is marked or not *)

let forward = ref false 
let comment = ref "#"
let decl = ref "%"
let keep_comments = ref true 
(*.dot*)

let boolean_site_color = ref "yellow" 
let boundable_site_color = ref "cyan" 
let both_site_color = ref "green" 
let agent0 = ref "lightgrey"
let agent1 = ref "lightgrey"
let agent2 = ref "lightgrey"
let agent3 = ref "lightgrey"
let agentn = ref "lightgrey"

let only_detect_cycles = ref false 

let node_color n = 
  match n 
  with 0 -> !agent0
  | 1 -> !agent1
  | 2 -> !agent2
  | 3 -> !agent3
  | _ -> !agentn

let site_shape = "circle" 
let site_size  = 5

let node_shape n = 
  match n with 
    0 -> "rectangle"
  | 1 -> "ellipse"
  | 2 | 3  -> "hexagon"
  | _ -> "octagon"


let simplxname = "SIMulator by PLectiX:  simplx "^Data.version
let complxname = "COMpressor by PLectiX: complx "^version 
let sepname = "\n"
let headline = ["This file has been automatically computed by the PLECTIX Compressor prototype.";simplxname;complxname;"Git commit date is "^date_commit]

let head = ref ("\n\n ******************************************************************************************\n 
"^(List.fold_right (fun a b -> a^"\n"^b) headline "")^
"******************************************************************************************\n")
let foot = head

let do_maximal_refinement = ref true
let do_dag_refinement = ref true 


let options = List.rev

(*Actions*) 

    [
  "--do-all",
  Multi(["--compute-local-views";
	  "--enumerate-complexes";
	  "--build-influence-map";
	  "--compute-qualitative-compression";
	  "--compute-quantitative-compression";
	  "--do-low-res-contact-map";
	  "--do-high-res-contact-map";
	  "--do-marshalling";
	  "--do-LATEX";
	  "--generate-ODE";
	  "--integrate-ODE";
	  "--do-HTML";
	  "--do-XML";
	  "--do-refine-to-force-cycles";
          "--do-compute-dag-refinement-relation";
          "--do-compute-maximal-refinement-relation"],[]),
          "launch everything",["0_Actions"],Normal;
      "--reset-all",
  Multi([
	"--no-compute-local-views";
	"--no-enumerate-complexes";
	"--no-build-influence-map";
	"--no-do-low-res-contact-map";
	"--no-do-high-res-contact-map";
	"--no-compute-qualitative-compression";
	"--no-compute-quantitative-compression";
	"--no-do-marshalling";
	"--no-do-LATEX";
	"--no-do-HTML";
	"--no-do-XML";
	"--no-generate-ODE";
	"--no-integrate-ODE";
	"--no-do-refine-to-force-cycles";
	"--no-do-compute-dag-refinement-relation";
	"--no-do-compute-maximal-refinement-relation"
      ],[]),
  "launch nothing",["0_Actions"],Normal;
  "--truc",Void,"",["0_Actions"],Normal;
  "--compute-local-views",Bool do_local_views,"compute reachability analysis",["0_Actions";"Reachability analysis"],Normal;
  "--enumerate-complexes",Bool do_enumeration,"enumerate complexes",["0_Actions";"Concretization"],Normal;
  "--build-influence-map",Bool do_influence,"construct influence maps",["0_Actions";"Influence map"],Normal;
  "--do-low-res-contact-map", Bool do_low_res_contact_map,"construct the low resolution contact map",["0_Actions";"Contact map"],Normal;
  "--do-high-res-contact-map", Bool do_high_res_contact_map,"constrauct the high resolution contact map",["0_Actions";"Contact map"],Normal;  
 "--compute-qualitative-compression",Bool do_qualitative_compression,"simplify the rules",["0_Actions";"Compression"],Normal;
  "--compute-quantitative-compression",Bool do_quantitative_compression,"simplify the rules",["0_Actions";"Compression"],Normal;
  "--do-LATEX",Bool do_dump_latex,"dump the LaTeX style file and the LaTeX document for the list of rules",["0_Actions";"LATEX"],Normal;
  "--do-ODE",Bool do_ODE,"compute the ODE system",["0_Actions";"ODE"],Hidden;
  "--generate-ODE",Bool do_ODE,"compute the ODE system",["0_Actions";"ODE"],Normal;
  "--integrate-ODE",Bool integrate_ODE,"integrate the ODE_system",["0_Actions";"ODE"],Normal;
  "--do-refine-to-force-cycles",Bool force_cycle,"Refine the system to avoid polymere formation",["0_Actions";"Polymers prevention"],Normal;
  "--do-compute-dag-refinement-relation",Bool do_dag_refinement,"compute the DAG for the refinement relation",["0_Actions";"Refinement detection"],Normal; 
"--do-compute-maximal-refinement-relation",Bool do_maximal_refinement,"compute the most abstract refinement of each rule",["0_Actions";"Refinement detection"],Normal;

"--do-marshalling",Bool do_marshalling,"Dump mashallization",["0_Actions";"Marshalling"],Normal;
 "--do-HTML",Bool do_HTML_session,"Launch HTML desktop",["0_Actions";"HTML"],Normal;
 "--do-XML",Bool do_XML_session,"dump XML session",["0_Actions";"XML"],Normal;
"--truc",Void,"",["Reachability analysis";"Concretization";"Influence map";"Compression";"HTML";"XML";"Marshalling"],Normal;

(*0_Input*)
									    
 "--input-marshalling",String input_marshalling,
"Start computation from this marshallized state",["1_Input";"Marshalling"],Normal;

 "--focus-on",String input_focus_on,
"Focus contact maps around the given rules",
["1_Input";"Contact map"],Normal;
(*2_Output*)

									"--output-scheme",MultiExt [
"--output-latex-version","_plx_version.tex";
"--output-latex-stat","_plx_stat.tex";
"--output-latex-species","_plx_species.tex";
"--output-latex-fragment","_plx_fragments.tex";
"--output-latex-rules","_plx_rules.tex";
"--output-latex-rule-system","_plx.tex";
"--output-latex-sty","_plx.sty";
"--output-ODE-obs-latex","_plx_ODE_obs.tex";
"--output-ODE-latex","_plx_ODE_system.tex";
"--output-ODE-contact","_plx_ODE_contact.dot";
(*"--output-ODE-mathematica","_plx_ODE_system.nb";*)
"--output-ODE-octave-aux","_plx_ODE_system_aux.m";
"--output-ODE-octave-init","_plx_ODE_system_init.m";
"--output-ODE-octave-size","_plx_ODE_system_size.m";
"--output-ODE-octave-jacobian","_plx_ODE_system_jacobian.m";
"--output-ODE-octave-activity","_plx_ODE_system_activity.m";
"--output-ODE-octave-obs","_plx_ODE_system_obs.m";
"--output-ODE-octave","_plx_ODE_system.m";
"--output-ODE-alphabet","_plx_ODE_alphabet";
"--output-ODE-covering","_plx_ODE_covering";
"--output-ODE-covering-latex","_plx_ODE_covering.tex";
"--output-ODE-obs","_plx_ODE_obs";
"--output-ODE-obs-head","_plx_head.data";
"--output-ODE-data","_plx.data";
"--output-ODE-gplot","_plx_ODE.gplot";
"--output-ODE-png","_plx_ODE.png";
"--output-ODE-script","_plx_ODE.script";
"--output-ODE-xml","_plx_ODE.xml";
"--output-marshalling","_plx.marshalling";
"--output-influence-map-txt","_plx_influence_map.txt";
"--output-influence-map-dot","_plx_influence_map.dot";
(*"--output-influence-map-jpg","_plx_influence_map.jpg";*)
"--output-quantitative-compression","_plx_compressed_quantitative.ka";
"--output-qualitative-compression","_plx_compressed_qualitative.ka";
"--output-low-res-contact-map-dot","_plx_low_res_contact.dot";
"--output-low-res-contact-map-ps","_plx_low_res_contact.ps";
"--output-low-res-contact-map-jpg","_plx_low_res_contact.jpg";
"--output-low-res-contact-map-txt","_plx_low_res_contact.txt";
"--output-high-res-contact-map-dot","_plx_high_res_contact.dot";
"--output-high-res-contact-map-ps","_plx_high_res_contact.ps";
"--output-high-res-contact-map-jpg","_plx_high_res_contact.jpg";
"--output-high-res-contact-map-txt","_plx_high_res_contact.txt";
"--output-intermediate-encoding","_plx_ckappa.txt";
"--output-gathered-intermediate-encoding","_plx_ckappa_gathered.txt";
"--output-pretty-qualitative-compression","_plx_compressed_qualitative.txt";
"--output-pretty-quantitative-compression","_plx_compressed_quantitative.txt";
"--output-boolean-encoding","_plx_boolean_encoding.txt";
"--output-gathered-boolean-encoding","_plx_boolean_encoding_gathered.txt";
"--output-pack-constraints","_plx_site_constraints.txt";
"--output-reachable-complexes","_plx_reachables.txt";
"--output-specie-map","_plx_specie_map.txt";
"--output-xml","_plx.xml";
"--output-html","_plx.html";
"--output-refined-system","_plx_refinement.ka";
"--output_dag_ref_dot","_plx_dag_refinement_relation.dot";
"--output_dag_ref_jpg","_plx_dag_refinement_relation.jpg";
"--output_maximal_ref_dot","_plx_maximal_refinement_relation.dot";
"--output_maximal_ref_jpg","_plx_maximal_refinement_relation.jpg"],
"generic file name for output files",
["2_Output"],Normal;
"--truc2",Void,"help",["2_Output"],Normal;
"--output-quantitative-compression",String dump_quantitative_compression,"write the compressed quantitative compression in a .bng format",["2_Output";"Compression"],Normal;
"--output-qualitative-compression",String dump_qualitative_compression,"write the compressed qualitative compression in a .bng format",["2_Output";"Compression"],Normal;
 "--output-pretty-qualitative-compression",String output_pretty_qualitative_compression, "write the pretty compressed qualitative compression",["2_Output";"Compression"],Normal;
"--output-pretty-quantitative-compression",String output_pretty_quantitative_compression, "write the pretty compressed quantitative compression",["2_Output";"Compression"],Normal;
"--output-refined-system",String output_without_polymere, 
"write the refined system without polymeres",["2_Output";"Polymers prevention"],Normal;

"--output-influence-map-txt",String output_influence_map_txt_file,
    "write the causality map with .txt format",["2_Output";"Influence map"],Normal; 
"--output-influence-map-jpg",String output_influence_map_jpg_file,
    "draw the influcence map in a jpg file",["2_Output";"Influence map"],Normal;

"--output-influence-map-dot",String output_influence_map_dot_file,
  "write the causality map with .dot format",["2_Output";"Influence map"],Normal;
"--output-low-res-contact-map-dot",String output_low_res_contact_dot_file,
    "write the low resolution contact map with a .dot format",["2_Output";"Contact map"],Normal;
"--output-low-res-contact-map-ps",String output_low_res_contact_ps_file,
    "write the low resolution contact map with a .ps format",["2_Output";"Contact map"],Normal;
"--output-low-res-contact-map-jpg",String output_low_res_contact_jpg_file,
    "write the low resolution contact map with a .jpg format",["2_Output";"Contact map"],Normal;
"--output-low-res-contact-map-txt",String output_low_res_contact_txt_file,
    "write the low resolution contact map with a .txt format",["2_Output'";"Contact map"],Normal;
"--output-high-res-contact-map-dot",String output_high_res_contact_dot_file,
    "write the high resolution contact map with a .dot format",["2_Output'";"Contact map"],Normal;
"--output-high-res-contact-map-ps",String output_high_res_contact_ps_file,
    "write the high resolution contact map with a .ps format",["2_Output'";"Contact map"],Normal;
"--output-high-res-contact-map-jpg",String output_high_res_contact_jpg_file,
    "write the high resolution contact map with a .jpg format",["2_Output'";"Contact map"],Normal;
"--output-high-res-contact-map-txt",String output_high_res_contact_txt_file,
    "write the high resolution contact map with a .txt format",["2_Output'";"Contact map"],Normal;
"--output-intermediate-encoding",String output_cbng,
    "write the intermediate encoding", ["2_Output'";"Intermediar encoding"],Expert;
"--output-gathered-intermediate-encoding",String output_gathered_cbng,
    "write the intermediate endoding (gathered)",["2_Output'";"Intermediar encoding"],Expert;
"--output-boolean-encoding",String output_boolean_encoding,
    "write the boolean encoding with isolated rules",["2_Output'";"Boolean encoding"],Expert;
"--output-gathered-boolean-encoding",
  String output_gathered_boolean_encoding,
  "write the boolean encoding with isolated rules (gathered)",
  ["2_Output'";"Boolean encoding"],Expert;
"--output-ODE-contact",
  String output_ODE_contact,
  "write the annotated(for ODE) contact map in a dot file",
  ["2_Output'";"ODE"],Normal;
"--output-ODE-mathematica",
  String output_ODE_mathematica,
  "write the ODE system in a mathematica file",
  ["2_Output'";"ODE"],Normal;
"--output-ODE-octave",
  String output_ODE_octave,
  "write the ODE system in a octave file",
  ["2_Output'";"ODE"],Normal;
"--output-ODE-octave-activity",
  String output_ODE_octave_activity,
  "write the activity of rules in a octave file",
  ["2_Output'";"ODE"],Normal;
"--output-ODE-octave-init",
  String output_ODE_octave_init,
  "write the initial concentration of fragments in a octave file",
  ["2_Output'";"ODE"],Normal;    
"--output-ODE-octave-obs",
  String output_ODE_octave_obs,
  "write the activity of observables in a octave file",
  ["2_Output'";"ODE"],Normal;

"--output-ODE-octave-aux",
  String output_ODE_octave_aux,
  "write the ODE auxilliary functions in a octave file",
      ["2_Output'";"ODE"],Normal;
"--output-ODE-octave-jacobian",
      String output_ODE_octave_jacobian,
      "write the ODE jacobian  in a octave file",
      ["2_Output'";"ODE"],Normal;
"--output-ODE-octave-size",
  String output_ODE_octave_size,
  "write the dimmension of the ODE in a octave file",
  ["2_Output'";"ODE"],Normal;
"--output-ODE-latex",
  String output_ODE_latex,
  "write the ODE in latex mode",
  ["2_Output'";"ODE";"LATEX"],Normal;
"--output-ODE-gplot",
      String output_ODE_gplot,
      "write the ODE gplot script",
      ["2_Output'";"ODE"],Normal;
"--output-ODE-png",
      String output_ODE_png,
      "write the ODE graphs",
      ["2_Output'";"ODE"],Normal;

"--output-ODE-script",
      String output_ODE_script,
      "write a script to (re)grenerate the plot",
      ["2_Output'";"ODE"],Normal;

"--output-ODE-obs-latex",
  String output_ODE_obs_latex,
  "write the observable in latex mode",
  ["2_Output'";"ODE";"LATEX"],Normal;
"--output-latex-sty",
  String output_latex_sty,
  "write the style file for a model",
  ["2_Output'";"LATEX"],Normal;
"--output-ODE-alphabet",
  String output_ODE_alphabet,
  "write the ODE alphabet",
  ["2_Output'";"ODE"],Normal;
"--output-ODE-covering",
  String output_ODE_covering,
  "dump the covering classes for each agent type",
  ["2_Output'";"ODE"],Normal;
"--output-ODE-covering-latex",
  String output_ODE_covering_latex,
  "dump the covering classes for each agent type",
  ["2_Output'";"ODE";"LATEX"],Normal;
"--output-ODE-obs",
  String output_ODE_obs,
  "write the set of obervables tracked in the ODE",
  ["2_Output'";"ODE"],Normal;
"--output-ODE-obs-head",
  String output_ODE_obs_head,
  "write the preamble of the data file",
  ["2_Output'";"ODE"],Normal;
"--output-ODE-data",  String output_ODE_data,
  "write plots coordinate",
  ["2_Output'";"ODE"],Normal;
"--output-ODE-xml", String output_ODE_xml,
  "write datas in an XML output",
  ["2_Output'";"ODE"],Normal;
"--output-pack-constraints",
 String output_pack_value_file,
 "dump contraints among sites in a file",["2_Output'";"Reachability analysis"],Normal;
"--output-reachable-complexes",String output_reachable_complexes,
  "write the reachable species (or just their number)  in a file",["2_Output'";"Concretization";"Reachability analysis"],Normal;
"--output-specie-map",String output_specie_map,
  "write the specie map in a file",["2_Output'";"Reachability analysis"],Normal;
"--output_dag_ref_dot",String output_dag_ref_dot,
"dump the dag-like refinement relation in a dot file",
["2_Output'";"Refinement detection"],Normal;
"--output_dag_ref_jpg",String output_dag_ref_jpg,
"dump the dag-like refinement relation in a jpg file",
["2_Output'";"Refinement detection"],Normal;
"--output_maximal_ref_dot",String output_maximal_ref_dot,
"dump the maximal refinement relation in a dot file",
["2_Output'";"Refinement detection"],Normal;
"--output_maximal_ref_jpg",String output_maximal_ref_jpg,
"dump the maximal refinement relation in a jpg file",
["2_Output'";"Refinement detection"],Normal;
"--output-latex-rule-system",String output_latex_rule_system,
"dump the rules in latex format",
["2_Output'";"LATEX"],Normal;
"--output-latex-version",String output_latex_version,
"dump the version information in latex format",
["2_Output'";"LATEX"],Normal;
"--output-latex-stat",String output_latex_stat,
"dump the computation stat in latex format",
["2_Output'";"LATEX"],Normal;
"--output-latex-fragment",String output_latex_fragment,
"dump the number of fragments in latex format",
["2_Output'";"LATEX"],Normal;
"--output-latex-rules",String output_latex_rules,
"dump the number of rules in latex format",
["2_Output'";"LATEX"],Normal;
"--output-latex-species",String output_latex_species,
"dump the number of species in latex format",
["2_Output'";"LATEX"],Normal;
"--output-xml",String output_xml,
    "write an xml file",["2_Output'";"XML"],Normal;
"--output-html",String output_html,
    "write an html file",["2_Output'";"HTML"],Normal;
"--output-marshalling",String output_marshalling,
    "marshallize the computation state",["2_Output'";"Marshalling"],Normal;

"--truc3",Void,"",["Compression";"Contact map";"Intermediar encoding";"Boolean encoding";"Reachability analysis";"Marshalling";"Influence map"],Normal;


"--memory-limit",Int memory_limit,"Limit the usage of the memory in (Mb)",["Memory usage"],Normal;

(*Compression*)
"--max-lens-size",Int max_lens_size,"Ignore rules with more than n complexes in their rhs",["Compression"],Normal;
"--no-comment-in-compression",
    Bool keep_comments,
      "put comments in the compression bng file",["Compression"],Normal;

"--ignore-dep",Bool ignore_dep,"enable any compression",["Compression"],Expert;
 "--dump-concrete-rules",Bool trace_concrete_rules,"to dump rules before compression",["Compression"],Normal;
 "--dump-abstract-rules",Bool trace_abstract_rules,"to dump rules after compression",["Compression"],Normal;

(*Concretization*)
 
   "--dump-all-complexes",Bool dump_all_complexs,"to dump concretization",["Concretization"],Normal;
   "--sort-complexes",Bool sort_complexes,"to sort complexes before dumping them",["Concretization"],Normal;
  "--complex-limit",Int complex_limit,"Enumerate complex only when there are less complexes",["Concretization"],Normal;

(*Contact map *)
   "--find-cycles",Bool find_potential_cycles,"to compute all cycles in the contact map",["Contact map"],Normal;
   "--find-connected-components",Bool find_connected_components,"to compute connected components",["Contact map"],Normal;

   "--display-unreachable-rules-in-high-res-contact-map",Bool display_unreachable_rules_in_contact_map,"to display unreachable rules in the drawers attached to agents, sites, and bonds in the high resolution contact map",["Contact map"],Normal;

(*Debug *)
   
   "--trace",Bool trace,"to dump debuging information",["3_Debug"],Expert;
   "--trace_iteration_number",Bool trace_iteration_number,"Dump rule Id before interpreting it",["3_Debug"],Normal;
   "--unsafe-mode",Bool unsafe_mode,"to keep on computation after unexpected hehavior",["3_Debug"],Expert;
   "--version",Bool dump_version,"to dump version number",["3_Debug"],Normal;

(*External applications*)
  "--html-browser",String html_browser,"comand line for launching an html browser",["External applications";"HTML"],Normal;


(*Influence map*)
  "--wake-up-map",Bool wake_up,"build wake up relations",["Influence map"],Normal;  
  "--inhibition-map",Bool inhibition,"build inhibition map",["Influence map"],Normal;

(*Latex*)

  "--latex-session-title",String latex_session_title,"Name of the session for computation log output in latex",["LATEX"],Normal;

(*ODE*)

  "--initial-time",Float ode_init_time,"initial time for ODE integration",["ODE"],Normal;
  "--final-time",Float ode_final_time,"final time for ODE integration",["ODE"],Normal;
  "--initial-step",Float ode_init_step,"initial time step for ODE integration",["ODE"],Normal;
  "--flat-ode",Bool flat_ode,"Compute the ODE for the flat system",["ODE"],Normal;
  "--plots",Int ode_points,"number of plots in the data file",["ODE"],Normal;
  "--ode-memoization-level",Int ode_memoization_level,
 "tune the level of memoization \n     0 -- no memoization \n     1 -- few memoization \n     2 -- much memoization",["ODE"],Expert;
  "--ode-output-mode", String ode_output_mode,
 "language for post-processing the output, only OCTAVE or MATLAB are available yet",["ODE"],Normal;

(*Packing*)
  "--auto-packs",Bool auto_packs, "use automatic packing",["Reachability analysis"],Normal;
    "--abstract-away-relations-between-sites",Bool site_abstraction,"Abstract away any relation between sites",["Reachability analysis"],Normal;
  "--abstract-away-relations-between-phosphorilation-and-binding",Bool  phospho_abstraction,"to abstract away any relation between phosphorilation and binding",["Reachability analysis"],Normal;
   "--abstract-away-information-about-phosphorilation",	Bool  ignore_linkage,
	"to abstract away information about binding",["Reachability analysis"],Normal;
   "--abstract-away-information-about-binding",
 	Bool  ignore_phospho,
	"to abstract away information about phosphorilation",["Reachability analysis"],Normal;

(*Refinment*)

  "--cycles-depth",Int cycle_depth,"Define the neighbourhood in which an agent can test for cycles",["Contact map";"Polymers prevention"],Normal; 
  "--cycle-detection-mode",Bool only_detect_cycles,"Show warning, but do not refine rules",["Polymers prevention"],Normal;
  "--use-constraints-to-refine",Bool only_closing_rules,"Only dump the rules that close a cyclical complex",["Polymers prevention"],Normal;
  "--refine-only-these-rules",String refine_fic,"Only refine these rules",["1_Input";"Polymers prevention"],Normal;
  "--kinetic-amplifier",Float kinetic_amplifier,"Multiply the rate of rules that close complexes",["Polymers prevention"],Normal;
(*Semantics*)
"--forward",
    Bool forward, "ignore reciproque reactions",["Semantics"],Normal;
(*Std output *)
   "--dump-rule-iteration",Bool trace_rule_iteration,"to dump the number of rules when iterating",["Standard output"],Normal;
   "--dump-iteration-number",Bool trace_iteration_number,"to dump whole iteration number",["Standard output"],Normal;




(*Reachability analysis*)

"--refine-after-instanciation",Bool refine_after_instanciation,
"enforce structural invariant after instanciation",["Reachability analysis"],Expert;
"--refine-after-guard",Bool refine_after_guard,
"enforce structural invariant after testing guard",["Reachability analysis"],Expert;
"--refine-after-control",Bool refine_after_control,
"enforce structural invariant after applyint control",["Reachability analysis"],Expert;
"--refine-during-export",Bool refine_during_export,
"enforce structural invariant when exporting the invariant",["Reachability analysis"],Expert;

(*key *)

"--key",String key,"security key",["Reachability analysis"],Hidden;

(*
(*Pretty print*)
"--site-separator",String site_separator,"site separator",["Pretty printing"],Normal;
"--solution-separator",String solution_separator,"solution separator",["Pretty printing"],Normal;
"--created-species",String created_species,"separator between regular agents and created/removed ones",["Pretty printing"],Expert;
"--mark_symbol",String mark_symbol,"mark symbol",["Pretty printing"],Normal;
"--bound-symbol",String bound_symbol,"bound symbol",["Pretty printing"],Normal;
"--free-symbol",String free,"to be printed when a site is free",["Pretty printing"],Normal;
"--bound-or-not",String bound_or_not,"to be printed when we do not know when a site is bounded, or not",["Pretty printing"],Normal;
"--bound_undefined",String bound_undefined,"to be printed when a site is not a binding site",["Pretty printing"],Normal;
"--bound_abstracted",String bound_abstracted,"to be printed when the binding of a site is not considered by the abstract domain",["Pretty printing"],Normal;
"--unmark",String unmark,"to be print whaen a site has no mark",["Pretty printing"],Normal;

"--comment",String comment,"to be print before adding a comment",["Pretty printing"],Expert;
"--decl",String decl,"to be print before adding a declaration",["Pretty printing"],Expert;
*)

(*Colors and shapes*)

"--truc",Void,"",["Contact map"],Normal;

"--boolean-site-color",String boolean_site_color,"color of sites that cannot be bound",["Contact map"],Normal;
"--boundable-site-color",String boundable_site_color,"color of sites that cannot be marked",["Contact map"],Normal;
"--both-site-color",String both_site_color,"color of sites that can be both marked and bound",["Contact map"],Normal;
"--agent0",String agent0,"color of agent that have no site",["Contact map"],Normal;
"--agent1",String agent1,"color of agent that have one site",["Contact map"],Normal;
"--agent2",String agent2,"color of agent that have two sites",["Contact map"],Normal;
"--agent3",String agent3,"color of agent that have three sites",["Contact map"],Normal;
"--agentn",String agentn,"color of agent that have many sites",["Contact map"],Normal;
    ] 

