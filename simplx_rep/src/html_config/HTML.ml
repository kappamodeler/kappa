open Data
open Mods2
open Simulation2
open Network
open Rule
open Error
open Printf

let rename prefix file ext = 
  let file_name = Filename.basename (chop_extension file) in
    (prefix^file_name^"."^ext)

let change_ext file ext =
  (chop_extension file)^"."^ext

let image_of_dot ?(neato=false) file = 
  let image_file = change_ext file Config.dot_image_format in
    if neato then
      Sys.command (sprintf "%s -T%s %s -o %s" Config.neato_command Config.dot_image_format file image_file) 
    else
      Sys.command (sprintf "%s -T%s %s -o %s" Config.dot_command Config.dot_image_format file image_file) 
      
let dot_of_network ?(story=true) ?(limit=false) fic (net,nb,time) = 
  if limit && (net.Network.fresh_id > !Data.network_display_limit)
  then 
    let d = open_out fic in
    let _ = fprintf d "digraph G{\n label=\"This story is too big to be displayed\"}" in
    let _ = close_out d in () 
  else
    
    let label e = 
      match Rule.flag e.r with
	None -> if e.kind = 0 then e.label else (e.r.Rule.input)
      | Some flg -> (flg)
    in
    let sort_events_by_depth events =
      let dp = IntMap.empty in
      EventArray.fold (fun id e dp -> 
	let l = try IntMap.find e.s_depth dp with Not_found -> [] in
	IntMap.add e.s_depth ((id,e)::l) dp
	  ) events dp
    in
    let d = open_out fic 
    and dp = sort_events_by_depth net.events
    in
    try
      fprintf d "digraph G{\n" ;
      if story then
	let time_str = if nb = 1 then "once at" else sprintf "%d times after an average of" nb in 
	fprintf d "label=\"Observed %s %.4ft.u (from %s)\"\n" time_str time (!Data.fic) 
      else
	fprintf d "label=\"Configuration observed at %.4ft.u (from %s)\"\n" time (!Data.fic) ;
      let weight_map = 
	IntMap.fold (fun depth l map ->
	  let fontcolor = if depth>9 then "white" else "black" in
	  fprintf d "{";
			 (*fprintf d "rank=same;\n";*)
	  fprintf d 
	    "node[fontcolor=\"%s\",color=\"black\",style=\"filled\"];\n" 
	    fontcolor;
	  let map =
	    List.fold_right
	      (fun (id,e) map ->
		let fillcolor,shape = 
		  match e.kind with
		    0 -> ("lightblue","invhouse")
		  | 1 -> (Palette.grey 3,"invhouse")
		  | _ -> ("red","ellipse") 
		in
		let lab = label e in
		fprintf d "\"[%s]_%d\" [shape=%s,fontsize=10,fillcolor=%s];\n" 
		  lab id shape fillcolor ;
		let set = try IntMap.find id net.s_preds with Not_found -> IntSet.empty in
		IntSet.fold (fun id' map -> 
		  try (
		    let e' = event_of_id id' net in
		    let weight = depth - e'.s_depth in 
						       (*negative value to have an increasing map*)
		    let cont = try IntMap.find weight map with Not_found -> [] in
		    IntMap.add weight ((id',id)::cont) map) 
		  with _ -> 
		    (print_string "BUG_HTML.ml";print_int id;print_string "->";print_int id';print_newline ();map)
		      ) set map
		  ) l map
	  in
	  fprintf d "}\n" ;
	  map
	    ) dp IntMap.empty
      in
      let preds_star = (*IntMap.fold*) 
	EventArray.fold (fun i e preds_star -> 
	  let set = preds_closure net (IntSet.singleton i) in
	  IntMap.add i set preds_star
	    ) net.events IntMap.empty 
      in
      let _ = 
	IntMap.iter (fun w l ->
	  List.iter (fun (i,j) ->
	    let preds_j = try IntMap.find j net.s_preds with Not_found -> IntSet.empty in
	    let keep = if not !Data.closure then true else 
	    try
	      IntSet.fold (fun k keep -> 
		if k=i then keep 
						       else
		  let set_k = 
		    try IntMap.find k preds_star 
		    with Not_found -> IntSet.empty in
		  if IntSet.mem i set_k then raise False
		  else keep
		      ) preds_j true
	    with False -> false
	    in
	    if keep then
	      let style,shape = ("filled","right")
	      in
	      fprintf d  "\"[%s]_%d\"->\"[%s]_%d\" [style=%s,dir=%s]\n" 
		(label (event_of_id i net)) i (label (event_of_id j net)) j style shape 
		) l
	    ) weight_map 
      in
      let _ =
	IntMap.iter (fun j w_preds_j  -> 
	  IntSet.iter (fun i ->
	    let preds_j = try IntMap.find j preds_star with Not_found -> IntSet.empty in
	    if IntSet.mem i preds_j then ()
	    else 
	      let style,shape = ("dotted","right")
	      in
	      fprintf d  "\"[%s]_%d\"->\"[%s]_%d\" [style=%s,dir=%s]\n" 
		(label (event_of_id i net)) i (label (event_of_id j net)) j style shape 
		) w_preds_j 
	    ) net.w_preds 
      in
      fprintf d "}\n" ;
      close_out d
    with
	exn -> (close_out d; raise exn)

(*TODO: tester l'extension du fichier et faire un output KAPPA ou DOT*)
let dot_of_solution sol fic =
  let d = open_out fic in
  let low_reso = (Solution.AA.size sol.Solution.agents) > 1000 in
  let label = String.sub fic 0 (String.index fic '.') in
  let label = if low_reso then label^" (low resolution mode)" else label in
    try
      fprintf d "digraph G{\n" ;
      fprintf d "label = \"%s\"" label ;
      fprintf d "%s" (Solution.to_dot ~low_reso:low_reso sol);
      fprintf d "}\n" ;
      close_out d
    with
	exn -> (close_out d ; raise exn)

(*ploting simulation data*)

let dot_of_conflict sim_data = 
  let l = 
    Rule_of_int.fold (fun i (r_i,act_i) cont -> 
			let flag = match r_i.flag with Some s -> s | _ -> r_i.input in
			let obs = IntSet.mem i sim_data.obs_ind in
			let dead = not (IntMap.mem i sim_data.flow) in
			let shape = 
			  if obs then "[shape=invhouse,color=black,style=filled,fillcolor=red]" 
			  else 
			    if dead then "[shape=doubleoctagon,style=filled,color=red,fillcolor=grey]" 
			    else "[shape=invhouse,style=filled,color=black,fillcolor=gray]" 
			in
			let str = sprintf "\"%s\" %s" flag shape
			in str::cont
		     ) sim_data.rules []
  in
  let l = 
    IntMap.fold (fun i set cont ->
		   let ri,_ = 
		     try Rule_of_int.find i sim_data.rules 
		     with Not_found -> 
		       let s = "Simulation2.dot_of_flow: rule indice not valid" in
		       runtime
			 (Some "HTML.ml",
			  Some 180,
			  Some s)
			 s
		   in
		     IntSet.fold (fun j cont ->
				    let rj,_ = 
				      try Rule_of_int.find j sim_data.rules 
				      with Not_found -> 
					let s= "Simulation2.dot_of_flow: rule indice not valid" in
					runtime
					  (Some "HTML.ml",
					   Some 191,
					   Some s)
					  s
				    in
				    let flag_i = match ri.flag with Some s -> s | _ -> ri.input 
				    and flag_j = match rj.flag with Some s -> s | _ -> rj.input 
				    in
				    let str = sprintf "\"%s\"->\"%s\" [arrowhead=tee]" flag_i flag_j
				    in
				      str::cont
				 ) set cont
		) sim_data.conflict l
  in
    sprintf "digraph G{%s}\n" (String.concat "\n" l)

let dot_of_flow ?(merge=false) sim_data = 
  let l = 
    Rule_of_int.fold (fun i (r_i,act_i) cont -> 
			let flag = match r_i.flag with Some s -> s | _ -> r_i.input in
			let obs = IntSet.mem i sim_data.obs_ind in
			let dead = not (IntMap.mem i sim_data.flow) in
			let shape = 
			  if obs then "[shape=invhouse,color=black,style=filled,fillcolor=red]" 
			  else 
			    if dead then "[shape=doubleoctagon,style=filled,color=red,fillcolor=grey]" 
			    else "[shape=invhouse,style=filled,color=black,fillcolor=gray]" 
			in
			let str = sprintf "\"%s\" %s" flag shape
			in str::cont
		     ) sim_data.rules []
  in
  let l =
    IntMap.fold (fun i set cont ->
		   let ri,_ = 
		     try Rule_of_int.find i sim_data.rules 
		     with Not_found -> 
		       let s = "Simulation2.dot_of_flow: rule indice not valid" in
		       runtime
			 (Some "HTML.ml",
			  Some 230,
			  Some s)
			 s
		   in
		     IntSet.fold (fun j cont ->
				    let rj,_ = 
				      try Rule_of_int.find j sim_data.rules 
				      with Not_found -> 
					let s = "Simulation2.dot_of_flow: rule indice not valid" in
					runtime 
					  (Some "HTML.ml",
					   Some 241,
					   Some s)
					  s
				    in
				    let flag_i = match ri.flag with Some s -> s | _ -> ri.input 
				    and flag_j = match rj.flag with Some s -> s | _ -> rj.input 
				    in
				    let str = 
				      sprintf "\"%s\"->\"%s\"" flag_i flag_j 
				    in
				      str::cont
				 ) set cont
		) sim_data.flow l
  in
  let l = 
    if merge then
      IntMap.fold (fun i set cont ->
		     let ri,_ = 
		       try Rule_of_int.find i sim_data.rules 
		       with Not_found -> 
			 let s = "Simulation.dot_of_flow: rule indice not valid" in
			 runtime 
			   (Some "HTML.ml",
			    Some 264,
			    Some s)
			   s
			 
		     in
		       IntSet.fold (fun j cont ->
				      let rj,_ = 
					try Rule_of_int.find j sim_data.rules 
					with Not_found -> 
					  let s = "Simulation.dot_of_flow: rule indice not valid" in
					  runtime
					    (Some "HTML.ml",
					     Some 276,
					     Some s)
					    s
				      in
				      let flag_i = match ri.flag with Some s -> s | _ -> ri.input 
				      and flag_j = match rj.flag with Some s -> s | _ -> rj.input 
				      in
				      let str = sprintf "\"%s\"->\"%s\" [arrowhead=tee]" flag_i flag_j
				      in
					str::cont
				   ) set cont
		  ) sim_data.conflict l
    else l
  in
    sprintf "digraph G{%s}\n" (String.concat "\n" l)


let print_data data_file rules data_map obs_ind time_map (*concentrations*) = 
  let d = open_out data_file in
  let entete =
    IntSet.fold (fun i cont -> 
		   let r,_ = try Rule_of_int.find i rules with Not_found -> 
		     let s= "Session.output_data" in
		     Error.runtime 
		       (Some "HTML.ml",
			Some 301,
			Some s)
		       s in 
		   let s = 
		     match r.flag with 
			 Some flg -> flg 
		       | None -> let s = "HTML.print_data" in
			 runtime
			   (Some "HTML.ml",
			    Some 304,
			    Some s)
			   s
		   in
		     s::cont
		) obs_ind [] 
  in
    Printf.fprintf d "#t %s\n" (String.concat " " entete) ;
    IntMap.iter (fun t m (*tmap,n*) -> 
		   let time = 
		     (* JF if !time_mode then string_of_float ((float_of_int t) *. !time_sample) 
		     else*) string_of_float (IntMap.find t time_map)
		   in
		   let l = IntSet.fold (fun i cont -> 
					  let v_i = IntMap.find i m in
					    (string_of_float v_i)::cont
				       ) obs_ind [] 
		   in
		   let str = String.concat " " (time::l) 
		   in
		     Printf.fprintf d "%s\n" str  
		) data_map (*concentrations*) ;
    close_out d ;
    if !Config.auto_plot or !html_mode then 
      let plot_file = ((chop_extension data_file)^".gplot") in
      let d = open_out plot_file in
      let term = 
	if !Config.auto_plot 
	then Config.gnuplot_window_terminal else Config.gnuplot_image_terminal 
      in
	Printf.fprintf d "set xlabel 'Time'\n" ;
	Printf.fprintf d "set ylabel 'Concentration/Activity'\n" ;
	Printf.fprintf d "set term %s \n" term ;
	Printf.fprintf d "set autoscale\n" ;
	if not !Config.auto_plot then fprintf d "set output '%s'\n" (change_ext data_file Config.gnuplot_image_terminal) ;
	let args = 
	  let rescale_str = 
	    if (!rescale < 1.0) or (!rescale > 1.0)
	    then [Printf.sprintf "rescale=%.4f" !rescale] else [] in
	  let sample_str = 
	    if !time_mode then [Printf.sprintf "sample=%.4ft.u" !time_sample]
	    else [Printf.sprintf "sample=%d event(s)" !step_sample]
	  in
	  let add_str = [!fic] in
	    String.concat " " (add_str@rescale_str@sample_str)
	in
	let date = 
	  let tm = Unix.localtime (Unix.time ()) 
	  in
	    Printf.sprintf "%d/%d/%d" 
	      tm.Unix.tm_mday tm.Unix.tm_mon (tm.Unix.tm_year + 1900) 
	in
	  Printf.fprintf d "set title '%s %s'\n" date args ;
	  let _ = 
	    List.fold_left (fun c title ->
			      let style = 
				if String.contains title '[' then "w l"
				else "w p"
			      in
				if c = 2 then (
				  Printf.fprintf d "plot '%s' using 1:%d title '%s' %s\n" data_file c title style;
				  c+1
				)
				else (
				  if not !Config.auto_plot then 
				    Printf.fprintf d "set output '%s'\n" (change_ext data_file Config.gnuplot_image_terminal) ;
				  Printf.fprintf d "replot '%s' using 1:%d title '%s' %s\n" data_file c title style;
				  c+1
				))
	      2 entete 
	  in
            close_out d ;
	    let error_code =
	      if !Config.auto_plot then 
		Sys.command ("gnuplot -persist "^plot_file)
	      else 
		Sys.command ("gnuplot "^plot_file) 
	    in 
	      match error_code with
		  0 -> ()
		| _ -> 
		    let s = "HTML.print_data: gnuplot error" in
		    runtime
		      (Some "HTML.ml",
		       Some 394,
		       Some s)
		      s

  
(* Stories to html *)

(* for links and divs *)
let anchor link link_name = "<a href=\""^link^"\">"^link_name^ "</a>"

let div style contents = "<div class=\""^style^"\">"^contents^"</div>\n"

let entete_html fic_name title =
"
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n
<html>
<head>
<title>
"
^fic_name^
"
</title>
<link rel=\"stylesheet\" type=\"text/css\" href=\""^Config.css_file^"\">
</head>
<body>
<h1>
"
^title^
"</h1>\n"

let final_html = 
"
</body>
</html>
"

let print_tab_html d l nb =
  let n_tot = nb in
  let l = List.fast_sort (fun (_,s,v,_) (_,s',v',_) -> 
			    let cmp = compare s s' in
			    if cmp = (-1) then cmp 
			    else if cmp = 0 then -(compare v v') else cmp  
			 ) l
  in
  let l_shift = if (l=[]) then [] else ("","",0.0,0.0)::(List.rev (List.tl (List.rev l))) 
  in
    fprintf d "<div class=\"down\"><table><tbody>";
    fprintf d "<tr><th>Story number</th><th>Observable</th> <th>Percentage</th><th>Average occurrence time</th><th>Thumbnail</th></tr>"
    ;  
    List.iter2 (fun (name,obs,v,t) (name',obs',v',t') -> 
		  fprintf d
"<tr><td><a href=\"%s.%s\">%s</a></td>
<td><b>%s</b></td>
<td>%.1f</td>
<td>%.4f</td>
<td><a href=\"%s.%s\"><img border=\"0px\" src=\"%s.%s\" alt=\"%s\" width=\"200px\" height=\"60px\"></a></td>
</tr>" 
		    (Filename.basename name) Config.dot_image_format (Filename.basename name) 
		    (if (obs=obs') then "" else obs) 
		    ((100.0 *. v) /.n_tot) (*((100.0 *. v)/.n_tot)/.v_sum*) 
		    t 
		    (Filename.basename name) Config.dot_image_format (Filename.basename name) Config.dot_image_format obs 
      ) l l_shift ;
    fprintf d "</tbody></table></div>\n" 
      

let build_html html_file influence data rules compression_log final c =
  let d = open_out html_file in
  let cpt = ref 0 in
  let l = 
    if !story_mode then printf "-Compiling stories (may take a while)...\n" ; flush stdout ;
    Hashtbl.fold (fun _ nets_nb l ->
		    List.fold_right (fun (net,i,time) cont -> 
				       let fic_name = Filename.concat !output_dir  ("h"^(string_of_int (!cpt)^".dot")) in
				       let obs = Network.obs_story net 
				       in
					 dot_of_network fic_name (net,i,(time/.(float_of_int i)));
					 let _ = image_of_dot fic_name in
					 let n = !cpt in
					   cpt:=n+1 ;
					   (sprintf "h%d" n,
					    obs,float_of_int i,(time/.(float_of_int i)))::cont
				    ) nets_nb l
		 ) c.drawers.Iso.hsh []
  in
  let date = 
    let tm = Unix.localtime (Unix.time ()) 
    in
      sprintf 
        "%d/%d/%d [%dh%d]" 
	tm.Unix.tm_mday (tm.Unix.tm_mon +1) (tm.Unix.tm_year + 1900) tm.Unix.tm_hour tm.Unix.tm_min
  in
  let fic_name = (!fic)^" ["^date^"]" in
    fprintf d "%s" (entete_html fic_name ("Simplx dashboard: "^date));
    let buttons = 
      let button1 = [div "button" ("Source file:  "^(anchor 
						       (Filename.concat !base_dir (!fic)) 
						       (Filename.basename (!fic))))]
      and button2 =
	match influence with
	    Some influence_file -> 
	      [div "button" (anchor (Filename.basename (change_ext influence_file Config.dot_image_format)) "Influence map")]
	  | None -> []
      and button3 = []
      and button4 = [div "button" (anchor !xml_session "xml session")]
      and button5 = 
	match data with
	    Some data_file -> 
	      [div "button" (anchor (Filename.basename (change_ext data_file Config.gnuplot_image_terminal)) "plot")]
	  | None -> []
      and button6 =
	match rules with
	    Some rules_file -> [div "button" (anchor (Filename.basename rules_file) "rules")]
	  | None -> []
      and button7 =
	match compression_log with None -> []
	| Some compression_file  -> [div "button" (anchor (Filename.basename 
					(change_ext compression_file "html")) "compression log")] 
      in
	(button1@button2@button3@button4@button5@button6@button7)
	@(List.fold_left (fun cont (ptr,title) -> (div "button" (anchor ptr title))::cont) [] Config.additional_buttons)
    in
    let end_state = 
      match final with
	  Some final_file -> 
	    [div "button" (anchor (Filename.basename (change_ext final_file Config.dot_image_format)) "end state")] 
	| None -> []
    in
    let buttons = buttons@end_state in
      fprintf d "%s <p>" (String.concat " " buttons) ;
      if !story_mode then (
	let nb = float_of_int c.drawers.Iso.nb in
	  print_tab_html d l nb
      );
      fprintf d "%s" final_html;
      close_out d ;
      if Config.auto_launch_browser && not !Config.auto_plot then
	let _ = Sys.command (sprintf "%s %s &" Config.browser_command html_file) in ()
    

let html_of_rules rules_file rules = 
  let html_rules,_ = 
    List.fold_left (fun (cont,i) r_i ->
		       if r_i.input = "" then (cont,i) else
			 let rule_name = 
			   match r_i.flag with Some s -> s | _ -> ""
			 and rule_img = ( 
			   let name = Filename.concat !output_dir (sprintf "r%d.dot" i) in
			   let rule_dot = Rule.to_dot r_i in
			   let d = open_out name in
			     (fprintf d "%s" rule_dot ; close_out d) ;
			     let _ = image_of_dot name in
			       change_ext name Config.dot_image_format
			 )
			 in
			 let size = -1 in (*font size for rules*)
			 let entry = 
			   sprintf 

"<tr>
			   <td> <span style=\"color: rgb(153, 0, 0); font-weight: bold;\">%s</span></td>
      <td><font size=\"%d\"> %s </td>
      <td> <a href=\"%s\"> <img border=\"0px\" src=\"%s\" alt=\"%s\" width=\"110px\" height=\"100px\"> </a> </td>
</tr>" rule_name size r_i.input (Filename.basename rule_img) (Filename.basename rule_img) rule_name
			 in
			   (entry::cont,i+1)
		    ) ([],0) rules 
  in
  let entete = entete_html rules_file (sprintf "%s: rules" (!fic)) in
  let d = open_out rules_file in
    fprintf d "<div class=\"down\"><table><tbody>";
    fprintf d "<tr><th>Rule name</th><th>Kappa syntax</th><th>Actions</th></tr>";
    fprintf d "%s\n%s\n%s" entete (String.concat "\n" html_rules) final_html ;
    fprintf d "</tbody></table></div>\n" ;
    close_out d
    


let html_of_compression_log log_file list = 
  let _ = print_string log_file in
  let _ = print_newline () in 
  let html_compression_log,_ = 
    List.fold_left 
      (fun (cont,i')  (before,after_weak,after_strong,i,j)  ->
	let before_compression_img = 
	  ( 
	  let name = Filename.concat  !output_dir (sprintf "story_before_compression_%d.dot" i') in
	  let _ = dot_of_network name ~limit:true  (before,i,j)  in
	  let _ = image_of_dot name in
	  change_ext name Config.dot_image_format
	    )
	in
	let after_weak_compression_img = 
	  ( 
	  let name = Filename.concat !output_dir (sprintf "story_after_weak_compression_%d.dot" i') in
	  let _ = dot_of_network name  (after_weak,i,j)  in
	  let _ = image_of_dot name in
	  change_ext name Config.dot_image_format
			 )
	in

	let after_strong_compression_img = 
	  if !Data.log_strong_compression then 
	  ( 
	  let name = Filename.concat !output_dir (sprintf "story_after_strong_compression_%d.dot" i') in
	  let _ = dot_of_network name  (after_strong,i,j)  in
	  let _ = image_of_dot name in
	  change_ext name Config.dot_image_format
	    ) else "" 
	in
	let entry = 
	  if !Data.log_strong_compression then 
	    begin
	      sprintf 
		"<tr>
		<td> <a href=\"%s\"> 
                <img border=\"0px\" src=\"%s\"  width=\"110px\" height=\"100px\"> </a> </td>
		<td> <a href=\"%s\"> 
                <img border=\"0px\" src=\"%s\"  width=\"110px\" height=\"100px\"> </a> </td>
		<td> <a href=\"%s\"> 
                <img border=\"0px\" src=\"%s\"  width=\"110px\" height=\"100px\"> </a> </td>
		
		</tr>" 
		(Filename.basename before_compression_img) 
		(Filename.basename before_compression_img) 
		(Filename.basename after_weak_compression_img)
		(Filename.basename after_weak_compression_img)
		(Filename.basename after_strong_compression_img)
		(Filename.basename after_strong_compression_img)
	    end
	  else
	     begin
	      sprintf 
		"<tr>
		<td> <a href=\"%s\"> 
                <img border=\"0px\" src=\"%s\"  width=\"110px\" height=\"100px\"> </a> </td>
		<td> <a href=\"%s\"> 
                <img border=\"0px\" src=\"%s\"  width=\"110px\" height=\"100px\"> </a> </td>
		 </tr>" 
		(Filename.basename before_compression_img) 
		(Filename.basename before_compression_img) 
		(Filename.basename after_weak_compression_img)
		(Filename.basename after_weak_compression_img)
	     end
	in
	(entry::cont,i'+1)
		    )
      ([],0) list
  in
  let entete = entete_html log_file (sprintf "%s: compression log" (!fic)) in
  let d = open_out log_file in
  fprintf d "<div class=\"down\"><table><tbody>";
  fprintf d "<tr><th>Before compression</th><th>After weak compression</th>%s"
	     (if !Data.log_strong_compression then "<th> After strong compression</th></tr>" else "");
  fprintf d "%s\n%s\n%s" entete (String.concat "\n" html_compression_log) final_html ;
  fprintf d "</tbody></table></div>\n" ;
    close_out d
    

 

