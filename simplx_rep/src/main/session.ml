open Mods2
open Network
open Iso
open Rule
open Data
open Solution
open Printf

let rule_global_id r = None
let agent_global_id ag = None    

(*******************Nodes*********************)

(*Log*)
type log = log_entry PortMap.t 
(*entry_t: 0 (info) | 1 (Warning) | >1 && <3 (Error) |  >3 (internal)*)
and  log_entry = {entry_t:int; message:string ; line:int option; count: int ; pos: float ; code:int option}

let add_log_entry t msg ?line ?code log = 
  let entry,new_msg = 
    try
      let old = PortMap.find (t,msg) log in
      	({old with count = old.count+1; pos = Unix.gettimeofday()},false)
    with Not_found -> ({entry_t=t; message=msg;line=line;count=1;code=code;pos=Unix.gettimeofday()},true)
  in
  let _ = 
    match t with
	0 -> (prerr_string (msg^"\n") ; flush stderr)
      | 1 -> if new_msg then (prerr_string ("*Warning* "^msg^"\n") ; flush stderr) 
      | 2 -> (
	  let line_info = match line with Some i -> (" line "^(string_of_int i)) | None -> "" in
	    prerr_string (Printf.sprintf "Error:%s %s\n" line_info msg) ; flush stderr
	)
      | 3 -> (prerr_string ("Simplx runtime error: "^msg^"\nPlease report to jk@plectix.com\n") ; flush stderr)
      | _ -> () (*no message to display*)
  in
    PortMap.add (t,msg) entry log

(*COMPLX TYPES*)
(*
type log = (string*float) list
type message = string list
type output_channel = log*message 
*)

let convert_cplx_log (cplx_log,message) splx_log = splx_log (*TODO*)

let init_log() = 
  add_log_entry 0 (Printf.sprintf "\n**** Simulation by Plectix v%s ****\n" Data.version) PortMap.empty

let xml_of_log log =
  let entries = 
    PortMap.fold (fun (t,msg) (e:log_entry) cont ->
		    let str_t = 
		      match t with
			  0 | (-1) -> ["Type=\"INFO\""]
			| 1 -> ["Type=\"WARNING\""]
			| 2 -> ["Type=\"ERROR\""]
			| 3 -> ["Type=\"ERROR\""]
			| _ -> ["Type=\"INTERNAL\""]
		    and str_msg = [Printf.sprintf "Message=\"%s\"" msg]
		    and str_line =
		      match e.line with
			  None -> []
			| Some i -> [Printf.sprintf "Line=\"%d\"" i]
		    and str_code = 
		      match e.code with
			  None -> []
			| Some i -> [Printf.sprintf "Code=\"%d\"" i]
		    and str_count = [Printf.sprintf "Count=\"%d\"" e.count]
		    and str_pos = [Printf.sprintf "Position=\"%f\"" e.pos]
		    in
		    let entry_att = String.concat " " (str_t@str_msg@str_count@str_line@str_code@str_pos) in
		      FloatMap.add e.pos (Printf.sprintf "<Entry %s/>" entry_att) cont
		 ) log FloatMap.empty
  in
  let str_entries = String.concat "\n" (FloatMap.fold (fun _ str cont -> cont@[str]) entries []) in
    Printf.sprintf "<Log>\n%s\n</Log>" str_entries

(*Story*)

type story = (graph_node list) * (graph_connection list)
(*node_t: 0 "INTRO" | 1 "RULE" | >1 "OBSERVABLE"*) 
and graph_node = {node_id:int;
		  node_t:int;
		  node_text:string;
		  node_data:string;
		  node_name:string option;
		  node_globalId:int option;
		  node_depth:int option
		 }

and graph_connection = {fromNode:int; toNode:int ; relation:int option} (*relation: 0 (NEGATIVE) 1 (POSITIVE)*)

let xml_of_graph_nodes nodes =
  String.concat "\n"
    (List.fold_right (fun n cont -> 
			let name_str = 
			  match n.node_name with 
			      Some name -> [Printf.sprintf "Name = \"%s\"" name]
			    | None -> []
			and globId_str = 
			  match n.node_globalId with 
			      Some id -> [Printf.sprintf "GlobalId =\"%d\"" id]
			    | None -> []
			and depth_str = 
			  match n.node_depth with
			      Some d -> [Printf.sprintf "Depth=\"%d\"" d]
			    | None -> []
			and t_str = 
			  match n.node_t with
			      0 -> " Type = \"INTRO\""
			    | 1 -> " Type = \"RULE\""
			    | _ -> " Type = \"OBSERVABLE\""
			in
			let opt_str = String.concat " " (name_str@globId_str@depth_str) in
			  (Printf.sprintf 
			     "<Node Id = \"%d\" %s Text =\"%s\" Data=\"%s\" %s/>" 
                             n.node_id t_str n.node_text n.node_data opt_str)::cont
		     ) nodes [])

let xml_of_graph_connections edges = 
  String.concat "\n" 
    (List.fold_right (fun c cont ->
			let rel_str = match c.relation with
			    Some i -> if i=0 then " Relation =\"NEGATIVE\"" else " Relation=\"POSITIVE\""
			  | None -> ""
			in
			let str = 
			  Printf.sprintf "<Connection FromNode=\"%d\" ToNode=\"%d\"%s/>" c.fromNode c.toNode rel_str
			in
			  str::cont
		     ) edges [])


let graph_of_network net = 
(*  let net = Story_compressor.compress net 
      (!Data.story_compression_mode) 
      (!Data.story_compression_granularity) in *)
  let label e = 
    match Rule.flag e.r with
	None -> if e.kind = 0 then e.label else e.r.Rule.input
      | Some flg -> flg
  in
  let sort_events_by_depth events =
    let dp = IntMap.empty in
      (*IntMap.fold*) 
      EventArray.fold (fun id e dp -> 
			 let l = try IntMap.find e.s_depth dp with Not_found -> [] in
			   IntMap.add e.s_depth ((id,e)::l) dp
		      ) events dp
  in
  let dp = sort_events_by_depth net.events
  in
  let weight_map,graph_nodes = 
    IntMap.fold (fun depth l (map,nodes) ->
		   List.fold_right
		     (fun (id,e) (map,nodes) ->
			let nodes = {node_id=id;
				     node_t= e.kind;
				     node_text= (label e) ;
				     node_name = None ;
				     node_globalId = rule_global_id e.r ; 
				     node_depth= Some depth;
				     node_data = e.r.input}::nodes 
			in
			let set = try IntMap.find id net.s_preds with Not_found -> IntSet.empty in
			let map =
			  IntSet.fold (fun id' map ->
			    let e' = event_of_id id' net in
			    let weight = depth - e'.s_depth in 
					   (*negative value to have an increasing map*)
			    let cont = try IntMap.find weight map with Not_found -> [] in
			    IntMap.add weight ((id',id)::cont) map
			      				 ) set map
			in
			  (map,nodes)
		     ) l (map,nodes) 
		) dp (IntMap.empty,[])
  in
  let preds_star = (*IntMap.fold*)
    EventArray.fold (fun i e preds_star -> 
		       let set = preds_closure net (IntSet.singleton i) in
			 IntMap.add i set preds_star
		    ) net.events IntMap.empty 
  in
  let graph_connections = 
    IntMap.fold (fun w l cont ->
		   List.fold_right (fun (i,j) cont ->
				      let preds_j = 
					try IntMap.find j net.s_preds
					with Not_found -> IntSet.empty in
				      let keep = 
					if not !Data.closure then true 
					else
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
					if keep then {fromNode=i;toNode=j;relation=None}::cont
					else cont
				   ) l cont
		) weight_map []
  in
    (graph_nodes,graph_connections)


let xml_of_stories ?(deadlock=false) drawers =  
  let nb_tot = drawers.nb and hsh = drawers.hsh in
  let stories = 
    Hashtbl.fold (fun _ drawer cont ->
		    List.fold_right (fun (net,nb_iso_to_net,time) cont -> 
				       let i = float_of_int nb_iso_to_net in
				       let av_time = time /. i 
				       and nodes,edges = graph_of_network net 
				       and percentage = i *. 100.0 /. (float_of_int nb_tot)
				       in
				       let obs = if deadlock then "deadlock" else obs_story net in
				       let story_att_str = 
					 Printf.sprintf 
					   "<Story Percentage=\"%f\" Observable=\"%s\" Average=\"%f\" InitTime = \"%f\">"
					   percentage obs av_time !init_time
				       and nodes_xml = xml_of_graph_nodes nodes
				       and edges_xml = xml_of_graph_connections edges
				       and end_story = "</Story>"
				       in
					 (String.concat "\n" [story_att_str;nodes_xml;edges_xml;end_story])::cont
				    ) drawer cont
		 ) hsh []
  in
    String.concat "\n" stories
    

(*Simulation*)

let ls_of_simulation rules obs_ind time_map data_map curr_step curr_time =
  
  (*  let t_plots = Mods2.chrono 0.0 in*)
  let xml_plots = 
    IntSet.fold (fun obs_id cont ->
		   let r,_ = Rule_of_int.find obs_id rules in
		   let id_str = [""] (*[Printf.sprintf "Id=\"%d\"" obs_id]*)
		   and glob_id = 
		     match rule_global_id r with
			 Some i -> [Printf.sprintf "GlobalId=\"%d\"" i]
		       | None -> []
		   and type_str = 
		     if r.input = "" then ["Type=\"OBSERVABLE\""]
		     else ["Type=\"RULE\""]
		   and text_str = 
		     match r.flag with
			 Some flg -> [Printf.sprintf "Text=\"%s\"" flg]
		       | _ -> (prerr_string "Error: no flag defined for observable" ; 
			       ["Text=\"Error\""])
		   in
		   let xml_plot = Printf.sprintf "<Plot %s/>" (String.concat " " (id_str@glob_id@type_str@text_str)) in
		     Printf.sprintf "%s\n%s" xml_plot cont
		) obs_ind ""
  in
  let ls_data = IntMap.fold (fun t map ls -> 
			       let time = 
				 if !time_mode then ((float_of_int t) *. !time_sample) 
				 else IntMap.find t time_map
			       in
			       let str = 
				 Printf.sprintf "%s,%s\n" (Float_pretty_printing.string_of_float  time)
				   (String.concat ","
				      (IntMap.fold (fun _ v cont -> 
						      (string_of_int (int_of_float v))::cont
						   ) map []
				      )
				   )
			       in
			       	 LongString.concat str ls
			    ) data_map LongString.empty
  in
  let sim_name = []
  and sim_total_events = [Printf.sprintf "TotalEvents = \"%d\"" curr_step]
  and sim_total_time = [Printf.sprintf "TotalTime = \"%s\"" (Float_pretty_printing.string_of_float curr_time)]
  and sim_init_time = [Printf.sprintf "InitTime = \"%s\"" (Float_pretty_printing.string_of_float !init_time)]
  and time_sample = if !time_mode then [Printf.sprintf "TimeSample = \"%s\"" (Float_pretty_printing.string_of_float !time_sample)] else []
  and event_sample = if not !time_mode then [Printf.sprintf "EventSample = \"%d\"" !step_sample] else []
  and rescaling = 
    if !rescale < 1.0 or !rescale > 1.0 then [Printf.sprintf "Rescaling = \"%s\"" (Float_pretty_printing.string_of_float !rescale)]
    else []
  in
  let sim_att = 
    String.concat " " (sim_name@sim_total_events@sim_total_time@sim_init_time@time_sample@event_sample@rescaling)
  in
    (LongString.concat (Printf.sprintf "\n<Simulation %s>\n%s<CSV>\n<![CDATA[" sim_att xml_plots) LongString.empty,
     ls_data,
     LongString.concat "]]>\n</CSV>\n</Simulation>" LongString.empty)
      

(*Influence map*)

let xml_of_maps rules ?conflict flow = 
  let nodes = Rule_of_int.fold (fun id (r,_) nodes -> 
				  {node_id=id;
				   node_t= if r.input = "" then 2 (*obs*) else 1 (*rule*) ;
				   node_text= (match r.flag with None -> r.input | Some flg -> flg);
				   node_name = r.flag; 
				   node_globalId = rule_global_id r ; 
				   node_data=
				      (if r.input = "" then Solution.kappa_of_solution (Solution.fuse_cc r.lhs) 
				       else r.input) ;
				   node_depth = None
				  }::nodes
			       ) rules []
  and pos_edges = 
    IntMap.fold (fun id succ edges -> 
		   IntSet.fold (fun id' edges ->
				  {fromNode=id;toNode=id';relation=Some 1 (*pos*)}::edges
			       ) succ edges
		) flow []
  and neg_edges = 
    match conflict with 
	None -> []
      | Some conflict ->
	  IntMap.fold (fun id succ edges ->
			 IntSet.fold (fun id' edges ->
					{fromNode=id;toNode=id';relation=Some 0 (*neg*)}::edges
				     ) succ edges
		      ) conflict []
  in
    Printf.sprintf "<InfluenceMap>\n%s\n%s\n%s\n</InfluenceMap>" 
      (xml_of_graph_nodes nodes) (xml_of_graph_connections pos_edges) (xml_of_graph_connections neg_edges)

let xml_of_solution sol n = 
  let kappa_str = Solution.kappa_of_solution sol in
    try
      Printf.sprintf "<Species Kappa=\"%s\" Number=\"%d\">\n</Species>\n" kappa_str n (*xml_nodes xml_bonds*)
    with
	exn -> Error.runtime "Session.xml_of_solution"   

(*Returns a triple of long stream (ls1,ls2,ls3) where ls1 is the header of the xml file, ls2 is the long strings corresponding to 
the list of species in the solution, while ls3 is the closing tag of the xml tree*)
let ls_of_species species curr_time =
  let ls =
    Species.fold (fun _ spec_list ls ->
		    List.fold_left (fun ls (spec,n) -> 
				      let xml_str = xml_of_solution (Species.sol_of_spec spec) n in
					LongString.concat xml_str ls
				   ) ls spec_list
		 ) species LongString.empty
  in
    (LongString.concat (Printf.sprintf "<FinalState Time=\"%s\">\n" (Float_pretty_printing.string_of_float curr_time)) LongString.empty,
     ls,
     LongString.concat (Printf.sprintf "</FinalState>") LongString.empty)
  
    
(***************session**************)
let schema_loc = "\"http://plectix.synthesisstudios.com SimplxSession.xsd\""
let xmlns = "\"http://plectix.synthesisstudios.com/schemas/kappasession\""
let xmlns_xsi = "\"http://www.w3.org/2001/XMLSchema-instance\""

let finalize xml_file ?xml_content log code = 
(*  let _ = if (not !save_sim_data) && (!max_iter>1) then Sys.remove !serialized_sim_data_file else () in *)
(*JF I have commented the previous line since it was the cause of crashes in story mode when only one iteration was requested. The crash was due to the fact that the temporary files was reloaded later, I do not know why *)
  let _ = if !Mods2.bench_mode then Gc.print_stat stdout else () in 
  let commandLine = 
    let com = String.concat " " (Array.to_list Sys.argv) in
      [Printf.sprintf "CommandLine = \"%s\"" com]
  and inputFile = [Printf.sprintf "InputFile = \"%s\"" !fic]
  and timeStamp =
    let date = 
      let tm = Unix.localtime (Unix.time ())
      in
	Printf.sprintf "%d-%d-%dT%d:%d:%d.0Z" 
	  (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon+1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
    in
      [Printf.sprintf "TimeStamp =\"%s\"" date]
  and schema = [Printf.sprintf "xsi:schemaLocation=%s  xmlns=%s xmlns:xsi=%s" schema_loc xmlns xmlns_xsi]
  in
  let log = (* adding the exception that are caught by complx in the log *)
    List.fold_left 
      (fun log error -> 
        add_log_entry 2 (Error_handler.string_of_error error) log)
      log (!Error_handler.error_list) 
  in
  let rec dump_longstrings desc l =
    match l with
	[] -> ()
      | (ls_head,ls_core,ls_tail)::tl ->
	  (
	    LongString.printf desc ls_head ; Printf.fprintf desc "\n" ;
	    LongString.printf desc ls_core ; Printf.fprintf desc "\n" ;
	    LongString.printf desc ls_tail ;
	    dump_longstrings desc tl)
  in
  let d = open_out xml_file in
    Printf.fprintf d "<?xml version='1.0' encoding='utf-8'?><SimplxSession %s>\n" 
      (String.concat " " (commandLine@inputFile@timeStamp@schema)) ;
    let log = 
      if not !Data.skip_xml then
	begin
	  match xml_content with 
	      None -> log 
	    | Some (xml_map,xml_sol,xml_snapshots,xml_stories,xml_sim) ->
		let t_session = Mods2.gettime() 
		and log = add_log_entry 0 "-Outputting results in xml session..." log 
		and t_map = Mods2.gettime() 
		in  
		let _ = Printf.fprintf d "%s" (String.concat "\n" xml_map) in
		let log = add_log_entry (-1) (sprintf "-Building xml tree for influence map (if any): %f sec. CPU" (Mods2.gettime()-.t_map)) log in
		let t_snap = Mods2.gettime() in
		let _ = dump_longstrings d xml_snapshots in
		let log = add_log_entry (-1) (sprintf "-Building xml tree for snapshots (if any): %f sec. CPU" (Mods2.gettime()-.t_snap)) log in
		let t_fin = Mods2.gettime() in
		let _ = dump_longstrings d xml_sol 
		in
		let log = add_log_entry (-1) (sprintf "-Building xml tree for the final state (if any): %f sec. CPU" (Mods2.gettime()-.t_fin)) log in
		let t_story = Mods2.gettime() in
		let _ = Printf.fprintf d "%s" (String.concat "\n" xml_stories) in
		let log = add_log_entry (-1) (sprintf "-Building xml tree for stories (if any): %f sec. CPU" (Mods2.gettime()-.t_story)) log in
		let t_data = Mods2.gettime() in  
		let _ = dump_longstrings d xml_sim in
		let log = add_log_entry (-1) (sprintf "-Building xml tree for data points (if any): %f sec. CPU" (Mods2.gettime()-.t_data)) log in
		  add_log_entry 0 (sprintf "-Results outputted in xml session: %f sec. CPU" (Mods2.gettime()-.t_session)) log
	end
      else
	add_log_entry 1 "-Skipping xml session construction as required" log 
    in
      Printf.fprintf d "\n%s\n</SimplxSession>\n" (xml_of_log log) ;
      close_out d ;
            Gc.full_major() ; (*to collect remaining dead memory*)
      exit code

let output_data ?(with_gnuplot=false) data_file rules data_map obs_ind time_map (*concentrations*) = 
  let d = open_out data_file in
  let entete =
    IntSet.fold (fun i cont -> 
		   let r,_ = try Rule_of_int.find i rules with Not_found -> Error.runtime "Session.output_data" in
		   let s = 
		     match r.flag with 
			 Some flg -> flg 
		       | None -> Error.runtime "Session.output_data"
		   in
		     s::cont
		) obs_ind [] 
  in
    Printf.fprintf d "#t %s\n" (String.concat " " entete) ;
    IntMap.iter (fun t m (*tmap,n*) -> 
		   let time = 
		     if !time_mode then string_of_float ((float_of_int t) *. !time_sample) 
		     else string_of_float (IntMap.find t time_map)
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
    if with_gnuplot then 
      let plot_file = ((chop_extension data_file)^".gplot") in
      let d = open_out plot_file in
	Printf.fprintf d "set xlabel 'Time'\n" ;
	Printf.fprintf d "set ylabel 'Concentration/Activity'\n" ;
	Printf.fprintf d "set autoscale\n" ;
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
				  Printf.fprintf d "replot '%s' using 1:%d title '%s' %s\n" data_file c title style;
				  c+1
				)
			   ) 2 entete 
	  in
             close_out d 

let snapshot (curr_sol,curr_time,snapshot_counter) =
  let species = Species.of_sol curr_sol in
  let (ls_head,ls_core,ls_tail) = ls_of_species species curr_time in
  let d = open_out_bin (!serialized_snapshots^(string_of_int snapshot_counter)) in
    Marshal.to_channel d (ls_head,ls_core,ls_tail) [] ;
    close_out d 
