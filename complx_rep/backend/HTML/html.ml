(* 01/03/2007 *)
(* Static analysis of BNG systems*)
(* Jerome Feret pour PlectiX *)
(* output for XML *)
(* xml.ml *)
  
open Data_structures
open Pb_sig
open Tools
open Config_complx  

  
let foot () = 
  "<html>\n<head>\n<TITLE>COMPLX-session: "^Config_complx.time_stamp^"</TITLE></head>\n<body>"
									
let input_file () = List.fold_right (fun x s -> if s="" then x else s^" "^x) (!Config_complx.input_file) ""

let time_stamp () = Config_complx.time_stamp 
let file_ref () = !Config_complx.output_html
    
let link channel link title = 
  if link <> "" then 
    Printf.fprintf channel "<LI><A href=\"#%s\">%s</A></LI>\n" link title 

let anchor channel link title = 
  if link <> "" then 
    Printf.fprintf channel "<h2><A name=\"%s\">%s </A></h2>\n" link title 

let http channel link title = 
  if link <> "" then 

    Printf.fprintf channel "<LI><A href=\"%s\">%s </A></LI>\n" 
      (Tools.diff_prefix (file_ref ()) link)  title
      
let command_line_string  ()  =  
  let s = ref "" in 
    let _ = 
      Array.iter  (fun x  -> if (!s) = "" then (s:=x) else s:=((!s)^" "^x)) Sys.argv in
    !s

let command_line channel  =  
  let print s = Printf.fprintf channel s in 
    (fun () ->  link channel "Command_line" "Command_line"),
    (fun () -> let _ = anchor channel "Command_line" "Command_line" in 
                print "%s \n<Hr>\n" (command_line_string ()))
      
let bul channel = Printf.fprintf channel "<UL>\n" 

let eul channel = Printf.fprintf channel "</UL>\n"

let log_title (l,m) channel = 
  let print s = Printf.fprintf channel s in 
  (fun () -> let _ = link channel "Log" "Log" in ()),
  (fun () -> 
    if m = [] && l = [] then ()
    else 
      begin
	
	let _ = anchor channel "Log" "Log"  in 
	let _ = print "<TABLE>\n" in 
	
	let _ = List.iter 
	    (fun s -> print "  <TR>\n    <TD>\n      WARNING</TD><TD>       %s</TD>\n  </TR>\n" s) m in 
	let _ = 
	  match List.rev l with [] -> ()
	  | (s,t)::q  ->
	      let _ = list_fold
		  (fun (s,t) t' -> print "  <TR>\n    <TD>\n </TD><TD>       %s</TD>\n    <TD>%s  s.</TD>\n  </TR>\n" s (string_of_float (t-.t'));t)
		  q t in () in 
	
	(print "</TABLE>\n<Hr>\n")
      end)


let print_rules pb channel   = 
  match pb with None -> (fun () -> ()),(fun () -> ()) 
    | Some pb -> 
	begin
	  match pb.boolean_encoding with None -> (fun () -> ()),(fun () -> ())
	  | Some system -> 
	      let print s = Printf.fprintf channel s in 
	      (fun () -> 
		let _ = link channel "Rules" "Rules" in ()),
	      (fun () -> 
		let _ = anchor channel "Rules" "Rules" in 
		let _ = bul channel in 
		let _ = 
		  List.iter 
		    (fun rc -> 
		      List.iter 
			(fun case -> 
	      List.iter 
			    (fun id -> 
			      if not (id.Pb_sig.r_clone) then 
				print "<LI> %s: %s </LI>\n" 
				  (name_of_rule id)
                       id.Pb_sig.r_simplx.Rule.input)
			    case.Pb_sig.labels)
			rc.Pb_sig.rules)
		    (List.rev system.system) in
		let _ = eul channel in 
		print "\n<Hr>\n")
	end 

let precomputed_data channel  file string  = 
  if file = "" 
  then (fun () -> ()),(fun () -> ())
  else 
    (fun () -> http channel file string),
    (fun () -> ())

let null = (fun () -> ()),(fun () -> ())

let print_species_map pb channel   = 
  if is_specie_map  pb then 
    precomputed_data channel 
      (!Config_complx.output_specie_map) 
      "Views"
  else null

let output_xml channel   = 
  precomputed_data channel 
    (!Config_complx.output_xml) 
    "Xml session"

let output_ODE_xml pb channel   = 
  precomputed_data channel 
    (!Config_complx.output_ODE_xml) 
    "Xml session (for ODE integration)"

let print_influence_map pb channel = 
  let file = !Config_complx.output_influence_map_jpg_file  in 
  if file = "" 
  then null
  else 
    precomputed_data channel file "influence map (jpg)"

      



let print_pack pb channel = 
    if is_sub_views pb then 
      precomputed_data channel 
	(!Config_complx.output_pack_value_file) 
	"Subviews"
	else null

let print_contact_map res pb channel = 
  match res with High -> 
    if is_contact_map res pb 
    then 
      precomputed_data channel 
	(!Config_complx.output_high_res_contact_jpg_file) 
	"Contact map (High resolution)"
    else 
      null
  | Low -> 
      if is_contact_map res pb
      then 
      precomputed_data channel (!Config_complx.output_low_res_contact_jpg_file)
       "Contact map (Low resolution)"
      else null

let print_contact_map_stoc pb channel = 
  if !Config_complx.stoc_ode 
  then 
    precomputed_data channel 
      (!Config_complx.output_stoc_contact_map_jpg_file) 
      "Contact map (Stochastic fragmentation)"
  else 
    null

let print_ODE_plot pb channel = 
  if (!Config_complx.integrate_ODE)
  then 
    precomputed_data 
      channel 
      (!Config_complx.output_ODE_png)
      "Differential trajectories (plot)"
  else 
    null 

let print_ODE_system_octave_data pb channel = 
  if (!Config_complx.integrate_ODE)
  then 
    precomputed_data 
      channel 
      (!Config_complx.output_ODE_data)
      "Differential trajectories (data)"
  else 
    null 

let print_ODE_system_octave_perturbation pb channel = 
  if (!Config_complx.do_ODE) or (!Config_complx.integrate_ODE)
  then 
    precomputed_data 
      channel 
      (!Config_complx.output_ODE_perturbation)
      "Perturbation (octave)"
  else 
    null 


let print_dag_refinement pb channel = 
  if is_dag_refinement_relation_jpg pb (!Config_complx.output_dag_ref_jpg)
  then 
    precomputed_data channel 
      (!Config_complx.output_dag_ref_jpg) 
      "DAG representation"
  else
    null

let print_maximal_refinement pb channel = 
  if is_maximal_refinement_relation_jpg pb (!Config_complx.output_maximal_ref_jpg)
  then 
    precomputed_data channel 
      (!Config_complx.output_maximal_ref_jpg) 
      "Maximal refinement relation"
  else
    null

let print_ODE_system_mathematica pb channel = 
  if !Config_complx.do_ODE or !Config_complx.integrate_ODE 
  then 
    precomputed_data channel 
      (!Config_complx.output_ODE_mathematica) 
      "Equations (mathematica)"
  else
    null 

let print_ODE_system_octave pb channel = 
  if !Config_complx.do_ODE or !Config_complx.integrate_ODE 
  then 
    precomputed_data channel 
      (!Config_complx.output_ODE_octave)
      "Main file (octave)"
  else
    null 

let print_ODE_system_octave_aux pb channel = 
  if !Config_complx.do_ODE or !Config_complx.integrate_ODE 
  then 
    precomputed_data channel 
      (!Config_complx.output_ODE_octave_aux)
      "Equations (octave)"
  else
    null 

let print_ODE_system_octave_activity pb channel = 
  if !Config_complx.do_ODE or !Config_complx.integrate_ODE 
  then 
    precomputed_data channel 
      (!Config_complx.output_ODE_octave_activity)
      "Rules activity (octave)"
  else
    null 

let print_ODE_system_octave_obs pb channel = 
  if !Config_complx.do_ODE or !Config_complx.integrate_ODE 
  then 
    precomputed_data channel 
      (!Config_complx.output_ODE_octave_obs)
      "Observable activity (octave)"
  else
    null 

let print_ODE_system_octave_init pb channel = 
  if !Config_complx.do_ODE or !Config_complx.integrate_ODE 
  then 
    precomputed_data channel 
      (!Config_complx.output_ODE_octave_init)
      "Initial solution (octave)"
  else
    null 

let print_ODE_system_octave_jacobian pb channel = 
  if !Config_complx.do_ODE or !Config_complx.integrate_ODE 
  then 
    precomputed_data channel 
      (!Config_complx.output_ODE_octave_jacobian)
      "Jacobian (octave)"
  else
    null 

let print_ODE_system_octave_size pb channel = 
  if !Config_complx.do_ODE or !Config_complx.integrate_ODE 
  then 
    precomputed_data channel 
      (!Config_complx.output_ODE_octave_size)
      "System size (octave)"
  else
    null 


let print_ODE_alphabet pb channel = 
  if !Config_complx.do_ODE or !Config_complx.integrate_ODE 
  then 
    precomputed_data channel 
      (!Config_complx.output_ODE_alphabet) 
      "Variables and activities"
  else
    null
 
let print_ODE_obs pb channel = 
  if !Config_complx.do_ODE or !Config_complx.integrate_ODE 
  then 
    precomputed_data channel 
      (!Config_complx.output_ODE_obs)
      "Observables"
  else
    null

let print_ckappa mode pb channel = 
  if mode = Unsmashed 
  then 
    if is_intermediate_encoding pb 
    then
    precomputed_data channel 
      (!Config_complx.output_cbng) 
	"Precompiled-Kappa file"
    else null
  else
    if is_gathered_intermediate_encoding pb
    then 
      precomputed_data channel
	(!Config_complx.output_gathered_cbng)
	"Precompiled-Kappa file (gathered)"
    else null


let print_boolean_encoding mode pb channel = 
  if mode = Unsmashed 
  then 
    if is_boolean_encoding pb 
    then 
    precomputed_data channel 
      (!Config_complx.output_boolean_encoding) 
      "Boolean encoding file"
    else null
  else
    if is_gathered_boolean_encoding pb 
    then	
      precomputed_data channel
	(!Config_complx.output_gathered_boolean_encoding)
	"Boolean encoding file (gathered)"
    else
      null

let print_reachables pb channel = 
    if is_reachables pb 
    then precomputed_data channel 
	(!Config_complx.output_reachable_complexes)
	"Reachable complexes"
    else null

let print_pretty_compression mode pb channel = 
   if mode = Full 
   then 
     if is_qualitative_compression pb
     then 
       precomputed_data channel 
	 (!Config_complx.output_pretty_qualitative_compression)
	 "Qualitative decontextualization (pretty printed)"
     else null
   else 
     if is_quantitative_compression pb 
     then 
       precomputed_data channel 
	 (!Config_complx.output_pretty_quantitative_compression)
	 "Quantitative decontextualization (pretty printed)"
     else null



let print_compressed_rules mode pb channel = 
  if mode = Full 
   then 
    if is_qualitative_compression pb 
    then 
     precomputed_data channel 
      (!Config_complx.dump_qualitative_compression)
      "Qualitative decontextualization (in a kappa file)"
    else null
  else
    if is_quantitative_compression pb 
    then 
    precomputed_data channel 
      (!Config_complx.dump_quantitative_compression)
      "Quantitative decontextualization (in a kappa file)"
    else null

let menutitle channel s = 
  (fun () -> Printf.fprintf channel "<h3> %s </h3>\n" s),
  (fun () -> ())
    
let dump_html pb channel (l,m)  = 
  let print s = Printf.fprintf channel s in 
  let _ = print "%s" (foot ()) in 
  let _ = print "<!-- Automaticaly generated by Complx %s -->\n" (Config_complx.version) in
  let _ = 
    List.iter 
      (print "%s <BR>\n")
      Config_complx.headline in 
  let _ = print "<Hr><em>Timestamp</em>:%s <BR>\n  <em>CommandLine</em>: %s <BR>\n <em>InputFile</em>:%s<BR>\n" 
    (time_stamp ()) 
    (command_line_string ()) 
    (input_file ()) in 
  let _ = print "<Hr>" in 
  let _ = print "<h1> Main menu </h1>\n" in 
  let task = 

    ([menutitle channel "Session info";
     command_line channel;
      log_title (l,m) channel;
      output_xml channel;
      menutitle channel "Plot and Drawings";
      print_contact_map Low pb channel ;
      print_contact_map High pb channel;
      print_contact_map_stoc pb channel;
      print_influence_map pb channel; 
      print_ODE_plot pb channel;

      menutitle channel "Kappa rules";
      print_rules pb channel ;
      print_ckappa Unsmashed pb channel ;
      print_ckappa Smashed pb channel ;
      print_boolean_encoding Unsmashed pb channel ;
      print_boolean_encoding Smashed pb channel ]
@(if !Config_complx.do_reaction && !Config_complx.output_reactions <> "" 
  then 
    [menutitle channel "Reactions";
    precomputed_data channel  (!Config_complx.output_reactions) "Network of reactions"; 
    precomputed_data channel  (!Config_complx.output_reactions_alphabet) "Table of species"]
  else [])
@[
      menutitle channel "Analysis result";
      print_contact_map Low pb channel ;
      print_contact_map High pb channel;
      print_contact_map_stoc pb channel;
      print_pack pb channel ;
      print_species_map pb channel; 
      print_reachables pb channel]@
     (if !Config_complx.do_influence && (!Config_complx.output_influence_map_txt_file<>"" or  !Config_complx.output_influence_map_dot_file<>"")
     then 
       [menutitle channel "Influence_map";
       precomputed_data channel (!Config_complx.output_influence_map_txt_file) "Influence map in txt";
       precomputed_data channel (!Config_complx.output_influence_map_dot_file) "Influence map in dot"] else [])@
     (if !Config_complx.do_qualitative_compression or !do_quantitative_compression 
     then [menutitle channel "Compression";
	    print_pretty_compression Full pb channel; 
	    print_compressed_rules Full  pb channel;
	    print_pretty_compression Isolated pb channel; 
	    print_compressed_rules Isolated  pb channel] else [])@
     (if is_refinement_relation_jpg pb (!Config_complx.output_dag_ref_jpg) (!Config_complx.output_maximal_ref_jpg)  
     then [menutitle channel "Refinement relation";
	    print_dag_refinement pb channel;
	    print_maximal_refinement pb channel]
     else [])@
      (if !Config_complx.force_cycle 
      then [menutitle channel "Refinement (to avoid polymer)";
	      precomputed_data channel 
	       (!Config_complx.output_without_polymere)
	       "Refinement"]
     else [])@
     (if (!Config_complx.do_ODE or !Config_complx.integrate_ODE) && (not !Config_complx.stoc_ode)
     then 
       [menutitle channel "Ordinary differential equations";
        output_ODE_xml pb channel;
	print_ODE_plot pb channel;
	print_ODE_system_mathematica pb channel;
	print_ODE_system_octave_data pb channel;
	print_ODE_system_octave pb channel;
	precomputed_data channel (!Config_complx.output_ODE_script)
	  "script";
	precomputed_data channel (!Config_complx.output_ODE_gplot)
	  "gnuplot file";
	print_ODE_system_octave_aux pb channel;
	print_ODE_system_octave_jacobian pb channel;
	print_ODE_system_octave_activity pb channel;
	print_ODE_system_octave_obs pb channel;
	print_ODE_system_octave_init pb channel;
        print_ODE_system_octave_perturbation pb channel;
	print_ODE_system_octave_size pb channel]
      else 
	[]))@
        (if !Config_complx.stoc_ode
         then 
            [menutitle channel "Stochastic fragmentation"; 
             print_contact_map_stoc pb channel;
             precomputed_data channel (!Config_complx.output_stoc_rules) "rules"
            ]
         else 
	    [])
      
  in 

      
  let _ = List.iter (fun a -> (fst a) ()) task in 
  let _ = print "<Hr>" in
  let _ = List.iter (fun a -> (snd a) ()) task in 
  let _ = print "</body>\n</html>\n" in 
  (l,m)
  
	
