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
open Data
open Key

let main ()  = 
  let prefix = empty_prefix  in 
  let _ = get_option () in 
  let _ = if try not (good_key !Config_complx.key) with _ -> true then failwith "This program cannot run standalone" else () in 
  let _ = trace_print "GOT OPTIONS" in 
  let methods = methods () in 
  let log = methods.empty_channel in 
  let log= if do_something () then 
          methods.print_headpage prefix log 
          else log in 
  let log = methods.dump_version prefix log in 
  let _ = trace_print "RETURN TR" in 
  let pb,log = 
    if !Config_complx.input_marshalling<>"" 
    then 
      let pb,log = methods.unmarshallize !Config_complx.input_marshalling  prefix  log in 
      methods.reset prefix pb log
    else 
      let simplx,log = methods.parse_file (List.hd !Config_complx.input_file) prefix log in
      let pb,log = methods.parse_line_by_line (List.hd !Config_complx.input_file) prefix simplx log in 
      pb,log in 
  let _ = trace_print "RETURN SIMPLX" in
  let pb,log = 
    if !Config_complx.do_low_res_contact_map 
    then methods.build_contact Low prefix pb log 
    else pb,log
  in
  let _ = trace_print "RETURN LOCAL" in 
  let pb,log = 
     if !Config_complx.do_local_views or !Config_complx.do_qualitative_compression 
	or !Config_complx.do_quantitative_compression
	
     then 
       let pb,log = methods.reachability_analysis prefix pb log in
       methods.refine_subviews prefix pb log 
     else pb,log in 
  let _ = trace_print "CONTACT MAP HIGH" in 
  let pb,log = 
    if !Config_complx.do_high_res_contact_map
    then methods.build_contact High prefix pb log
    else pb,log in 
  let _ = trace_print "CONTACT MAP LOW" in 
  let pb,log = 
    if !Config_complx.do_high_res_contact_map 
	or !Config_complx.do_low_res_contact_map 
    then
      methods.build_drawers prefix pb log 
    else pb,log in 
  let _ = trace_print "INFLUENCE:QUARK" in 
  let pb,log = 
    if !Config_complx.do_influence 
    then 
      methods.quarkification prefix pb log 
    else pb,log in 
  let _ = trace_print "INFLUENCE:INFLUENCE" in 
  let pb,log =
    if !Config_complx.do_influence
    then 
      methods.build_influence_map (!Config_complx.output_influence_map_txt_file) (!Config_complx.output_influence_map_dot_file) (!Config_complx.output_influence_map_jpg_file) prefix pb log 
    else pb,log in 
  let pb,log = 
    if (*!Config_complx.do_qualitative_compression 
	or !Config_complx.do_quantitative_compression
	or*) !Config_complx.do_enumeration
	or !Config_complx.do_local_views 
    then 
      methods.refine_views prefix pb log
    else
      pb,log in 
  
  let _ = trace_print "START QUALITATIVE COMPRESSION" in 
  let pb,log   = 
    if !Config_complx.do_qualitative_compression
    then 
      methods.build_compression Full (!Config_complx.dump_qualitative_compression) (!Config_complx.output_pretty_qualitative_compression) prefix pb  log 
    else
      pb,log
  in
  let _ = trace_print "END COMPRESSION" in 
  let _ = trace_print "START QUANTITATIVE COMPRESSION" in 
  let pb,log   = 
    if !Config_complx.do_quantitative_compression
    then 
      methods.build_compression Isolated (!Config_complx.dump_quantitative_compression) (!Config_complx.output_pretty_quantitative_compression)  prefix pb  log 
    else pb,log in
  let _ = trace_print "END COMPRESSION" in 
  let pb,log = 
   if !Config_complx.do_enumeration (*or !Config_complx.count_complexes*)
    then 
     methods.build_enumeration (!Config_complx.output_reachable_complexes) prefix pb  log 
  else pb,log in 
  let pb,log = 
    if !Config_complx.find_potential_cycles 
    then 
      methods.find_potential_cycles High prefix pb log 
    else
      pb,log in
  
  let pb,log = 
    if !Config_complx.dump_potential_cycles 
    then
      methods.dump_potential_cycles High prefix pb log 
    else
      pb,log in 
  let pb,log = 
    if !Config_complx.find_connected_components 
    then 
      methods.find_connected_components High prefix pb log
    else
      pb,log in 
  let pb,log =  methods.dump_boolean_encoding (!Config_complx.output_boolean_encoding) Unsmashed prefix pb log in 
  let pb,log =  methods.dump_ckappa (!Config_complx.output_cbng) Unsmashed prefix pb log in 
  let pb,log =  methods.dump_local_views (!Config_complx.output_specie_map) prefix pb log in 
  let pb,log =  methods.dump_packs_constraints (!Config_complx.output_pack_value_file) prefix pb log  in 
  let _ = correct_contact_map () in 
  let pb,log = methods.dump_contact_map_txt High (!Config_complx.output_high_res_contact_txt_file) prefix pb  log in 
  let pb,log = methods.dump_contact_map_dot High (!Config_complx.output_high_res_contact_dot_file) prefix pb log in 
  let pb,log = methods.dump_contact_map_ps High (!Config_complx.output_high_res_contact_ps_file) prefix pb log in 
  let pb,log = methods.dump_contact_map_jpg High (!Config_complx.output_high_res_contact_jpg_file) prefix pb log in 
 let pb,log = methods.dump_contact_map_txt Low (!Config_complx.output_low_res_contact_txt_file) prefix pb  log in 
  let pb,log = methods.dump_contact_map_dot Low (!Config_complx.output_low_res_contact_dot_file) prefix pb log in 
  let pb,log = methods.dump_contact_map_ps Low (!Config_complx.output_low_res_contact_ps_file) prefix pb log in 
  let pb,log = methods.dump_contact_map_jpg Low (!Config_complx.output_low_res_contact_jpg_file) prefix pb log in 

  let pb,log = methods.dump_boolean_encoding (!Config_complx.output_gathered_boolean_encoding) Smashed prefix pb log in 
  let pb,log = methods.dump_ckappa (!Config_complx.output_gathered_cbng) Smashed prefix pb log in 

  let pb,log = methods.save_options prefix pb log in 
  
  let pb,log = 
    if !Config_complx.do_reaction 
    then 
       methods.template_bis 
	(!Config_complx.output_reactions)
        (!Config_complx.output_ODE_perturbation)
	(!Config_complx.output_ODE_contact) 
        (!Config_complx.output_stoc_contact)
        (!Config_complx.output_stoc_rules)
	(!Config_complx.output_ODE_covering) 
	(!Config_complx.output_ODE_covering_latex)
	(!Config_complx.output_ODE_latex) 
	(!Config_complx.output_ODE_octave)  
	(!Config_complx.output_ODE_octave_aux) 
	(!Config_complx.output_ODE_octave_size)
	(!Config_complx.output_ODE_octave_jacobian)
	(!Config_complx.output_ODE_octave_activity)
	(!Config_complx.output_ODE_octave_obs)
	(!Config_complx.output_ODE_octave_init)
	(!Config_complx.output_ODE_mathematica) 
	"" 
	(!Config_complx.output_reactions_alphabet) 
	(!Config_complx.output_ODE_obs) 
	(!Config_complx.output_ODE_obs_latex)  
	(!Config_complx.output_ODE_obs_head) 
	""
	(!Config_complx.output_ODE_data)
	(!Config_complx.output_ODE_gplot)
	(!Config_complx.output_ODE_png)
	(!Config_complx.output_ODE_script)
        (!Config_complx.output_ODE_xml)
	prefix 
	pb 
	log 
    else 
      pb,log in 
  let pb,log = 
    if !Config_complx.do_ODE or !Config_complx.integrate_ODE or !Config_complx.do_ODE_matlab or !Config_complx.stoc_ode 
    then 
      methods.template 
        (!Config_complx.output_ODE_perturbation)
	(!Config_complx.output_ODE_contact) 
        (!Config_complx.output_stoc_contact)
        (!Config_complx.output_stoc_rules)
	(!Config_complx.output_ODE_covering) 
	(!Config_complx.output_ODE_covering_latex)
	(!Config_complx.output_ODE_latex) 
	(!Config_complx.output_ODE_octave)  
	(!Config_complx.output_ODE_octave_aux) 
	(!Config_complx.output_ODE_octave_size)
	(!Config_complx.output_ODE_octave_jacobian)
	(!Config_complx.output_ODE_octave_activity)
	(!Config_complx.output_ODE_octave_obs)
	(!Config_complx.output_ODE_octave_init)
	(!Config_complx.output_ODE_mathematica) 
	"" 
	(!Config_complx.output_ODE_alphabet) 
	(!Config_complx.output_ODE_obs) 
	(!Config_complx.output_ODE_obs_latex)  
	(!Config_complx.output_ODE_obs_head) 
	""
	(!Config_complx.output_ODE_data)
	(!Config_complx.output_ODE_gplot)
	(!Config_complx.output_ODE_png)
	(!Config_complx.output_ODE_script)
        (!Config_complx.output_ODE_xml)
	prefix 
	pb 
	log 
    else 
      pb,log in 
  let pb,log = 
    if (!Config_complx.output_ODE_matlab <> "" && !Config_complx.do_ODE_matlab) 
    then 
      methods.dump_ODE_matlab
	(!Config_complx.output_ODE_matlab) 
	prefix 
	pb 
	log 
    else pb,log
  in 
  let pb,log = 
    if !Config_complx.integrate_ODE 
    then 
      methods.integrate (!Config_complx.output_ODE_script) prefix pb log 
    else
      pb,log  in
  let pb,log = 
    if !Config_complx.do_dump_latex && (!Config_complx.output_latex_rule_system <> "") then 
      methods.dump_latex_rule_system 
	(!Config_complx.output_latex_rule_system) 
	prefix pb log 
    else pb,log 
  in 
  let _,_,log = 
    if !Config_complx.force_cycle or !Config_complx.only_detect_cycles then 
      let subsystem,(l,m) = 
	if  !Config_complx.refine_fic="" 
	then None,log
	else 
	  let simplx,log =
	    let s = !Config_complx.refine_fic in 
	    let _ = Data.forward:= (!Config_complx.forward) in 
	    let _ = Data.compile_mode:=true in 
	    if s = "" 
	    then None,log
	    else 
	      let (a,b,b2,c,d) = Kappa_lex.compile s  in 
	      (*let b = !Data.pairs in*) (*Correction JK*)
	      let _ = trace_print "COMPILATION DONE" in
	      Some (Some(a,b,!Data.init_l,!Data.obs_l,Experiment.unfun !Data.exp)),log 
		
	  in simplx,log 
      in
      methods.refine_system_to_avoid_polymers
	(!Config_complx.output_without_polymere)
	subsystem
	(match !Config_complx.only_detect_cycles,!Config_complx.only_closing_rules
	with 
	  false,true -> Avoid_polymere.Refine_with_constrains
	| false,false -> Avoid_polymere.Refine_without_constrains
	| true,_ -> Avoid_polymere.Warn)
	    (let a = !Config_complx.cycle_depth in if a<0 then None else Some a ) 
	(!Config_complx.kinetic_amplifier)
	prefix pb log
    else
      Some [],pb,log 
  in 
  let pb,log = 
    if !Config_complx.do_maximal_refinement 
    then
      methods.compute_refinement_relation_maximal prefix pb log 
    else
      pb,log in
  let pb,log = 
    if !Config_complx.do_dag_refinement 
    then
      let pb,log = methods.compute_refinement_relation_dag prefix pb log in
       pb,log
    else
      pb,log in
  let pb,log = methods.dump_dag_refinement_relation (!Config_complx.output_dag_ref_dot) (!Config_complx.output_dag_ref_jpg) prefix pb log in
   let pb,log = methods.dump_maximal_refinement_relation (!Config_complx.output_maximal_ref_dot) (!Config_complx.output_maximal_ref_jpg) prefix pb log in
   let pb,log = 
    if !Config_complx.do_marshalling
    then methods.marshallize (!Config_complx.output_marshalling) prefix pb log 
    else pb,log in 
 
   let _ = 
     methods.dump_latex_dictionary (!Config_complx.output_latex_sty) prefix pb log 
   in 
   let _ = 
     methods.dump_latex_version (!Config_complx.output_latex_version) prefix pb log in 
   let _ = 
     methods.dump_latex_stat (!Config_complx.output_latex_stat) prefix pb log in 
   let _ = 
     methods.dump_latex_fragments_number (!Config_complx.output_latex_fragment) prefix pb log in 
  
    let _ = 
     methods.dump_latex_rules_number (!Config_complx.output_latex_rules) prefix pb log in 
   
    let _ = 
     methods.dump_latex_species_number (!Config_complx.output_latex_species) prefix pb log in 

  let pb,log  = 
    if !Config_complx.do_XML_session
    then methods.dump_session  (!Config_complx.output_xml) prefix pb  log 
    else pb,log in 
  
  let pb,log  = 
    if !Config_complx.do_HTML_session
    then 
      (
      methods.dump_html_output (!Config_complx.output_html) prefix pb log) 
    else pb,log in 

 
  let _ = if do_something () then 
    let _  = methods.print_channel prefix log in  
    let _ = methods.print_errors prefix in 
    let _  = methods.print_footpage prefix log in 
      ()
  in
  let _ = 
    match !Error_handler_common.error_list 
    with 
        [] -> exit 0 
      | _ -> exit 1
  in 
  ()



let _ = main () 
