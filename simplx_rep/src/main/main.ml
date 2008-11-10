open Printf
open Error
open Mods2
open Simulation2
open Data
open Key
open Error_handler 

let usage_msg = "SimPlx "^version^": \n"^"Usage is simplx --[sim|compile|storify] file.ka "
let version_msg = "SIMulator by PLectiX: "^version^"\n"^key_version

let main =
  let options = [ 
    ("-W", Arg.Unit (fun () -> verbose:=true), "output all warnings on standard error channel (very verbose)") ;
    ("--sim", Arg.String (fun s -> fic := s), "name of the kappa file to simulate");
    ("--compile", Arg.String (fun s -> compile_mode:=true; fic := s), "name of the kappa file to compile");
    ("--storify", Arg.String (fun s -> story_mode := true ; fic := s), "name of the kappa file to storify");
    ("--generate-map", Arg.String (fun s -> map_mode :=true ; fic := s), "name of the kappa file for which the influence map should be computed");
    ("--version", Arg.Unit (fun () -> print_string (version_msg^"\n") ; flush stdout ; exit 0), "print simplx version");
    ("--time", Arg.Float (fun f -> time_mode:=true ; max_time := f), "(infinite): time units of computation");
    ("--event", Arg.Int (fun i -> time_mode :=false ; max_step := i), "(infinite): number of rule applications");
    ("--points", Arg.Int (fun i -> data_points := i), "number of data points per plots)");
    ("--iteration", Arg.Int (fun i -> max_iter := i), 
     "(1): number of stories to be searched for (with --storify option only)");
    ("--rescale", Arg.Float (fun f -> rescale := f), "(1.0): rescaling factor (eg. '10.0' or '0.10')") ;
    ("--init", Arg.Float (fun f -> init_time := f), "(0.0): start taking measures (stories) at indicated time");
    ("--output-final-state",Arg.Unit (fun () -> output_final:=true),"output final state") ;
    ("--no-inhibition-map",Arg.Unit (fun () -> build_conflict:=false),"do not construct inhibition map");
    ("--no-activation-map",Arg.Unit (fun () -> build_cause:=false),"do not construct activation map");
    ("--no-maps",Arg.Unit (fun () -> build_conflict:=false ; build_cause:=false),
     "do not construct inhibition/activation maps");
    ("--merge-maps",Arg.Unit (fun () -> merge_maps:=true),"also constructs inhibition maps");
    ("--output-scheme", Arg.String (fun s -> output_dir := Filename.concat (Filename.current_dir_name) s), 
     "(current dir) directory on which to put computed data"); 
    ("--set-snapshot-time", Arg.Float (fun f -> snapshot_mode:=true ; snapshot_time := (!snapshot_time)@[f]),
     "takes a snapshot of solution at specified time unit (may use option several times)");
    ("--no-arrow-closure",Arg.Unit (fun () -> closure:=false),
     "do not perform arrows transitive closure when displaying stories");
    ("--seed",Arg.Int (fun i -> seed:=Some i), "seed the random generator using given integer (same integer will generate the same random number sequence)");
    ("--no-measure", Arg.Unit (fun () -> ignore_obs:=true), "causes simplx to ignore observables") ;
    ("--quotient-refinements", Arg.Unit (fun () -> quotient_refs:=true), "replace each rule by the most general rule it is a refinement of when computing stories");
    ("--memory-limit",Arg.Int (fun i -> memory_limit:=i), "limit the usage of the memory (in Mb). Default is infinite (0)");
    (*expert mode options*)
    ("--max-clashes", Arg.Int (fun i -> max_clashes := i), "[expert] (infinite) max number of consecutive clashes before aborting"); 
    ("--key", Arg.String (fun s -> key := s), "[expert] name of the file containing the key for the crypted version");
    ("--save-map", Arg.String (fun s -> save_map:=true ; serialized_map_file := s), 
     "[expert] name of the file in which to save influence map");
    ("--load-map", Arg.String (fun s -> load_map:=true ; serialized_map_file := s), 
     "[expert] name of the serialized map file to load");
    ("--load-compilation", Arg.String (fun s -> load_compilation:=true ; serialized_kappa_file := s),
     "[expert] name of the serialized kappa file compilation to load");
    ("--save-compilation", Arg.String (fun s -> save_compilation:=true ; serialized_kappa_file := s), 
     "[expert] name of the file in which to save the kappa file compilation");
    ("--load-all", Arg.String (fun s -> load_sim_data:=true ; serialized_sim_data_file := s),
     "[expert] name of the serialized init file compilation to load");
    ("--save-all", Arg.String (fun s -> save_sim_data:=true ; serialized_sim_data_file := s), 
     "[expert] name of the file in which to save the whole initialization's marshalling (including influence maps)");
    ("--clock-precision",Arg.Int (fun i -> clock_precision:=i) , 
     "[expert] (60) clock precision (number of ticks per run)");
    ("--debug",Arg.Unit (fun () -> debug_mode:=true), "[expert] debug mode (very verbose!)");
    ("--QA",Arg.Unit (fun () -> sanity_check:=true), "[expert] QA mode (slower, but performs more sanity checks)");
    ("--profile",Arg.Unit (fun () -> Mods2.bench_mode:=true), "[expert] to produce profiling.");
    ("--gc-high"),Arg.Int (fun i -> if i>100 then gc_high:=1.0 else gc_high:=(float_of_int i)/.100.) , 
    "[expert] (90) triggers strong garbage collection when memory usage is above the given percentage of memory limit" ;
    ("--gc-low"),Arg.Int (fun i -> if i>100 then gc_low:=1.0 else gc_low:=(float_of_int i)/.100.) , 
    "[expert] (70) faster garbage collection when memory usage is below the given percentage of memory limit" ;
    ("--set-gc-overhead",Arg.Int (fun i -> gc_overhead:=i),"[expert] (80) tune the gc speed. Value below 80 will result in better gc but poorer performances and values above 80 will increase performances but result in big memory consumption") ;
    ("--snapshot-tmp-file", Arg.String (fun s -> serialized_snapshots:=s), 
     "[expert] set name for the temporary snapshots file (default ~tmp_snapshots)");
    ("--data-tmp-file", Arg.String (fun s -> serialized_data_file:=s), 
     "[expert] set name for the temporary file for serializing data (default ~tmp_data)");
    ("--xml-session-name",Arg.String (fun s -> xml_session:=s),
     "name of the xml file containing results of the current session (default simplx.xml)");
    ("--deadlock-threshold",
     Arg.Float (fun i -> deadlock_sensitivity:=i),"[expert] Defines the activity of a deadlocked system (default 0.0)");
    ("--plot-prob-intra", Arg.String (fun s -> plot_p_intra:=true ; p_intra_fic:=s), "[expert] Plot the evolution of the proba of intra during time in given file name");

    (*temporary options*)
    ("--light-xml", Arg.Unit (fun () -> skip_xml:=true), "[temporary] prevent simplx from building xml structures for results") ;
    ("--no-seed",Arg.Unit (fun () -> seed:=Some 0),"[temporary] equivalent to --seed 0. Kept for compatibilty issue") ; 
    ("--compress-stories",Arg.Unit (fun () -> story_compression:=true),"[temporary] weak compression of stories");
    ("--no-compress-stories",Arg.Unit (fun () -> story_compression:=false),"[temporary] do not compress stories");
    ("--use-strong-compression",Arg.Unit (fun () -> strong_compression:=true),
     "[temporary] use strong compression to classify stories");
    ("--no-use-strong-compression",Arg.Unit (fun () -> strong_compression:=false),
     "[temporary] do not use strong compression to classify stories");
    ("--forward",Arg.Unit (fun () -> forward:=true), "[temporary] do not consider backward rules" );
    ("--log-compression",Arg.Unit (fun () -> log_compression:=true),
     "[temporary] display the before/after compression status in the html desktop");
    ("--backtrack-limit",Arg.Int (fun i -> max_backtrack:=i),"[temporary] limit the exploration when scanning for stories");
    ("--max-time-per-compression",
     Arg.Float (fun i -> max_time_per_compression:=i),"[temporary] limit the exploration when scanning for stories");
    ("--reorder-by-depth",Arg.Unit (fun () -> reorder_by_depth:=true),
     "[temporary] reoder events according to their depth before strong compression");
    ("--use-multiset-order-in-compression",
     Arg.Unit (fun () -> reorder_by_depth:=true ; use_multiset_in_strong_compression:=true),
     "[temporary] use the multi-set of depths to compare stories in strong compression");
    ("--use-linear-order",Arg.Unit (fun () -> reorder_by_depth:=false;use_multiset_in_strong_compression:=false),
     "[temporary] use linear-order to compare stories in strong compression");
    ("--html-output",Arg.Unit (fun () -> html_mode:=true), "[temporary] html rendering") ;
    ("--dot-output",Arg.Unit (fun () -> dot_output:=true), "[temporary] dot output for stories") ;
    ("--no-rules",Arg.Unit (fun () -> Config.build_rules:=false), "[temporary] no recomputation of html rule rendering");
    ("--plot",Arg.String (fun s -> Config.auto_plot:=true; data_file:=s), 
     "[temporary] Creates a file containing the simulation data in clear text");
    ("--no-abstraction",Arg.Unit (fun () -> cplx_hsh:=false), 
     "[temporary] deactivate complx abstraction (will slow down influence map generation for large systems)") ;
    ("--no-random-time",Arg.Unit (fun () -> no_random_time:=true), "[temporary] use time advance expectency only")
  ]
  in
  let visible_options = List.filter (fun (_,_,s) -> 
				       not ((Mods2.option_type s "temporary") or (Mods2.option_type s "expert"))
				    ) options 
  in
    Arg.parse options (fun _ -> Arg.usage visible_options usage_msg ; exit 1) usage_msg ;
    if !fic = "" then (Arg.usage visible_options usage_msg ; exit 1) ;
    if not (good_key !key) then (exit 1) ;
    
    base_dir := Sys.getcwd();

    (*log creation*)
    let log = Session.init_log() in (*write simplx version, date and command line arguments in log*)
    let _ = 
      if not (Sys.file_exists !output_dir) then 
	begin
	  prerr_string ("Directory "^(!output_dir)^" does not exist, xml file not created.") ;
	  exit 2
	end 
    in
      (*end log creation*)
      
    let xml_file = Filename.concat !output_dir !xml_session in 
      
    (*Computing event or time sample based on arguments*)
    let log = 
      match (!max_step<0,!max_time<0.0) with
	  (true,true) -> 
	    begin
	      ignore_obs:=true ;
	      Session.add_log_entry 1 "No time or event limit defined with a potentially infinite simulation! No data point will be taken." log 
	    end
	| (true,false) -> 
	    time_sample:= (!max_time) /. (float_of_int !data_points) ; log
	| (false,true) ->
	    let sample =
	      let i=(!max_step)/(!data_points) in 
		if (i<1) then 1 else i 
	    in
	      step_sample:=sample ; log
	| (false,false) ->
	    begin
	      let sample =
		let i=(!max_step)/(!data_points) in 
		  if (i<1) then 1 else i 
	      in
		step_sample:=sample ;
		time_mode:=false ;
		Session.add_log_entry 1 "Both event and time limits are defined, using event limit for sampling" log
	    end
    in

    (*Setting up gc alarms for memory control*)
    let log = Memory_control.set_alarms log in

    let _ = 
      try 
	let log,warn,rules,init,sol_init,obs_l,exp =
	  if !load_sim_data then
	    let rules=[]
	    and init=[]
	    and sol_init = Solution.empty()
	    and obs_l=[]
	    and exp=Experiment.empty
	    in
	      (log,0,rules,init,sol_init,obs_l,exp)
	  else
	    let log,t_compil = 
	      if !load_compilation then 
		(Session.add_log_entry 0 (sprintf "-Loading %s..." !serialized_kappa_file) log,0.0)
	      else (Session.add_log_entry 0 "-Compilation..." log, Mods2.gettime()) 
	    in
	      if !load_compilation then 
		begin
		  let d = open_in_bin (!serialized_kappa_file) in 
		  let (f_r,f_i,f_s,f_o,e) = 
		    (Marshal.from_channel d:
		       (Rule.marshalized_t list 
			* ((Solution.marshalized_t * int) list) 
			* Solution.marshalized_t 
			* Solution.marshalized_obs list 
			* Experiment.t)
		    ) 
		  in
		    close_in d ; 
		    let r = List.map Rule.unmarshal f_r 
		    and i = List.map (fun (f_s,i) -> (Solution.unmarshal f_s,i)) f_i
		    and s = Solution.unmarshal f_s 
		    and o = List.map Solution.unmarshal_obs f_o
		    in
		    let log = Session.add_log_entry 0 
		      (sprintf "-%s succesfully loaded" !serialized_kappa_file) log
		    in
		      (log,0,r,i,s,o,e)
		end
	      else 
		(*compute rules,sol_init,obs_l and exp from a kappa file*)
		begin
		  try
		    let (r,i,o,e) = Kappa_lex.compile (!fic) in
		    let s = Solution.sol_of_init i in
		    let log = 
		      Session.add_log_entry 0 (sprintf "-Compilation: %f sec. CPU" (Mods2.gettime()-.t_compil)) log 
		    in
		      if !save_compilation then 
			let log = 
			  let file = Filename.concat !output_dir !serialized_kappa_file in
			  let d = open_out_bin file in
			    begin
			      Marshal.to_channel d 
				(List.map Rule.marshal r,List.map (fun (s,i) -> (Solution.marshal s,i)),
				 Solution.marshal s,List.map Solution.marshal_obs o,e) [] ;
			      let log = Session.add_log_entry 0 
				(sprintf "-%s succesfully saved" file) log
			      in 
				close_out d ;
				log
			    end
			in
	      		  (log,0,r,i,s,o,e)
		      else (log,0,r,i,s,o,e)
		  with
		      Error.Syntax (msg,line) -> 
			let log = Session.add_log_entry 2 msg ~line:line log in
			  (log,2,[],[],Solution.empty(),[],Experiment.empty)
		end
	in
	  if (warn > 1) or (!compile_mode) then 
	    begin
	      (************* TEMP RENDERING OF RULES***************)
	      if !html_mode && (warn < 2) && !Config.build_rules then (
		printf "-Compiling rule page (may take a while)...\n"; flush stdout ;
		let rules_file = Filename.concat !output_dir (HTML.rename "rules_" (!fic) "html") in
		  HTML.html_of_rules rules_file rules 
	      ) 
	      else 
		(
		  List.fold_right (fun r _ -> Rule.print_compil ~filter:((warn>1) && (not !compile_mode)) r) rules ();
		  Experiment.print exp ;
		  Printf.printf "INITIAL SOLUTION:\n" ;
		  List.iter (fun (sol,n) -> 
			       let str = Solution.kappa_of_solution sol in
				 Printf.printf "-%d*[%s]\n" n str
			    ) init
		) ;
	      (************* TEMP RENDERING OF RULES***************)
	      Session.finalize xml_file log warn  
	    end
	  else
	    if !story_mode && (not !load_sim_data) then 
	      if not (List.exists (fun obs -> match obs with Solution.Story _ -> true | _ -> false) obs_l)
	      then 
		let log = Session.add_log_entry 2 ("No story observation defined in "^(!fic)^". Aborting") log in
		  Session.finalize xml_file log 2
	      else () ;

	  let t_init = Mods2.gettime() in
	  let log = Session.add_log_entry 0 "-Initialization..." log in
	  let log = 
	    if (not !map_mode) then
	      match !seed with
		  None -> (Random.self_init() ;
			   let i = Random.int 1073741823 in (*max int32, should patch Random.int pour 64 bits*) 
			     seed:=Some i ;
			     Random.init i;
			     Session.add_log_entry 0 (Printf.sprintf "--Seeding random number generator with %d" i) log)
		| Some i -> (Random.init i ; Session.add_log_entry 0 ("--Seeding random number generator with given seed "^(string_of_int i)) log)
	    else log
	  in

	  let log,p_sd = 
	    (***********Loading simulation data from marshalized file**********)
	    if !load_sim_data then
	      begin 
		try 
		  let log = Session.add_log_entry 0 
		    (sprintf "--Loading initial state from %s..." !serialized_sim_data_file) log 
		  in
		  let d = open_in_bin (!serialized_sim_data_file) in 
		  let f_sd = (Marshal.from_channel d:Simulation2.marshalized_sim_data_t) in
		  let p = {max_failure = !Data.max_clashes;
			   init_sd = Some !serialized_sim_data_file ;
			   compress_mode = true ;
			   iso_mode = false ;
			   gc_alarm_high = false ;
			   gc_alarm_low = false 
			  }
		  in
		  let log = Session.add_log_entry 0 "--Initial state successfully loaded." log in
		    (log,Some(p,Simulation2.unmarshal f_sd))
		with 
		    _ -> 
		      let s = (Printf.sprintf "Could not load %s" !serialized_sim_data_file) in
		      Error.runtime
			(Some "main.ml",
			 Some 304,
			 Some s)
			s
	      end
	    else
	      (log,None) 
	  in 
	  let log,p,sd = 
	    match p_sd 
	    with 
		Some (p,sd) -> (log,p,sd)
	      | None -> 
		  begin 
		    (***************Creating simulation data*****************)
		    let log = Session.add_log_entry 0 "--Computing initial state" log 
		    in
		    let log,sd = Simulation2.init log (rules,init,sol_init,obs_l,exp)  
		    in
		    let log,serialized = 
		      if !save_sim_data or (!max_iter >1) then 
			let file = Filename.concat !output_dir !serialized_sim_data_file in
			let log = Session.add_log_entry 0 (sprintf "--Saving initial state to %s..." file) log 
			in
			let d = open_out_bin file in
			let f_sd = Simulation2.marshal sd in
			  begin
			    Marshal.to_channel d f_sd [] ;
			    let log = Session.add_log_entry 0 "--Initial state succesfully saved" log
			    in 
			      close_out d ;
			      (log,true)
			  end
		      else (log,false)
		    in
		      (log,{ max_failure = !Data.max_clashes;
			     init_sd = if serialized then Some (Filename.concat !output_dir !serialized_sim_data_file) else None;
			     compress_mode = true ;
			     iso_mode = false ;
			     gc_alarm_high = false ;
			     gc_alarm_low = false 
			   },sd
		      )
		  end 
	  in
	  let msg = sprintf "-Initialization: %f sec. CPU" (Mods2.gettime() -. t_init) in
	  let log = Session.add_log_entry 0 msg log in
	  let _ = Gc.compact() in (*collecting dead memory at the end of initialization*)
	    (*end initialization*)
	  let log,sd,p,c = 
	    if not !map_mode then
	      begin
		let log = Session.add_log_entry 0 "-Simulation..." log in
		let t_sim = Mods2.gettime() in
		let (deadlocked,log,sd,p,c) = iter log sd p (Simulation2.init_counters ()) in
		let log,drawers,compress_log  = 
		  Iso.compress_drawers log c.drawers p.iso_mode (fun a b c -> Session.add_log_entry a b c) 
		in 		 
		let c = 
		  {c with drawers = drawers ;
		     compression_log = compress_log } in 
		  
		let log = 
		  if (deadlocked=1) then 
		    (Session.add_log_entry 1 "-Simulation was interrupted because no rule could be applied anymore!" log)
		  else 
		    log
		in
		  (Session.add_log_entry 0 (sprintf "-Simulation: %f sec. CPU" (Mods2.gettime() -. t_sim)) log,
		   sd,p,c)
	      end 
	    else (log,sd,p,Simulation2.empty_counters)
	  in
	    if !Mods2.bench_mode then Bench.output() ;
	    
	    (*SESSION FINALIZATION*)
	    let xml_map,log = 
	      if !build_cause then 
		begin
		  let log = Session.add_log_entry 0 "-Outputting influence map" log in
		    if !merge_maps then 
		      begin
			(****************TEMP RENDERING OF INFLUENCE MAP*******************)
			if !html_mode then ( 
			  let flow_file = Filename.concat !output_dir (HTML.rename "influence_map_" (!fic) "dot") in
			  let d = open_out flow_file in (
			      fprintf d "%s" (HTML.dot_of_flow ~merge:true sd) ;
			      fprintf stderr "-Compiling %s...\n" flow_file ; flush stderr ;
			      close_out d ;
			      let _ = HTML.image_of_dot flow_file in () 
			    )
			) ;
			(******************************************************************)
			
			([Session.xml_of_maps sd.rules ~conflict:sd.conflict sd.flow],log)
		      end
		    else 
		      begin
			(****************TEMP RENDERING OF WAKE UP MAP*********************)
			if !html_mode then ( 
			  let flow_file = Filename.concat !output_dir (HTML.rename "influence_map_" (!fic) "dot") in
			  let d = open_out flow_file in (
			      fprintf d "%s" (HTML.dot_of_flow sd) ;
			      printf "-Compiling %s...\n" flow_file ; flush stdout ;
			      close_out d ;
			      let _ = HTML.image_of_dot flow_file in () 
			    ) 
			) ;
			(******************************************************************)
			
			([Session.xml_of_maps sd.rules sd.flow],log)
		      end
		end
	      else ([],log)
	    in
	    let ls_sol,log = 
	      if !output_final && not !story_mode then 
		begin
		  let log = Session.add_log_entry 0 "-Outputting final state as required" log in
		  let species = Species.of_sol sd.sol in

		    (************TEMP RENDERING OF FINAL STATE******************)
		    if !html_mode then (
		      let sol_file = Filename.concat !output_dir (HTML.rename "final_" (!fic) "dot") in
			HTML.dot_of_solution sd.sol sol_file ;
			printf "-Compiling %s (may take a while)...\n" sol_file ; flush stdout ;
			let _ =
			  HTML.image_of_dot ~neato:true sol_file in ()
		    ) ;
		    (***********************************************************)
		    let (ls_head,ls_core,ls_tail) = Session.ls_of_species species c.curr_time in
		      ([(ls_head,ls_core,ls_tail)],log)
		end
	      else ([],log)
	    in
	    let ls_snapshots,log = 
	      if !snapshot_mode then
		begin
		  let log = Session.add_log_entry 0 "-Outputting snapshots as requiered" log in
		  let rec gather_snapshots file counter (xmls,log) =
		    if counter < 0 then (xmls,log)
		    else
		      let xmls,log=
			try 
			  let current_file = file^(string_of_int counter) in
			  let d = open_in_bin current_file in
			  let (ls_head,ls_core,ls_tail) = 
			    (Marshal.from_channel d : (LongString.t*LongString.t*LongString.t))
			  in
			    close_in d ;
			    let msg = Printf.sprintf "-Snapshot file %d sucessfully unmarshalled" counter in
			    let log = Session.add_log_entry 1 msg log in
			      Sys.remove current_file ; 
			      ((ls_head,ls_core,ls_tail)::xmls,log)
			with Sys_error msg ->
			  let msg = Printf.sprintf "-Snapshot file %d could not be unmarshalled!" counter in
			  let log = Session.add_log_entry 2 msg log in
			    (xmls,log)
		      in
			gather_snapshots file (counter-1) (xmls,log)
		  in
		    gather_snapshots !Data.serialized_snapshots (c.snapshot_counter-1) ([],log)
		end
	      else ([],log)
	    in
	    let xml_stories,log = 
	      if !story_mode then 
		if c.curr_iteration - c.skipped = 0 then 
		  let log = Session.add_log_entry 1 "no stories found" log in
		  let drawers = Iso.empty_drawers 1 in
		  let drawers = Iso.classify (sd.net,c.curr_time) drawers false in
		    if !output_final then 
		      begin
			let log = Session.add_log_entry 0 "-Outputting deadlocked configuration as required" log in

			  (****************TEMP RENDERING OF STORIES************)
			  if !html_mode then 
			    (HTML.dot_of_network ~story:false "config_dump.dot" (sd.net,1,c.curr_time) ;
			     fprintf stderr "-Dumping final configuration (may take a while)...\n" ; flush stderr ;
			     let _ = HTML.image_of_dot "config_dump.dot" in ()
			    );
			  (*****************************************************) 

			  ([Session.xml_of_stories ~deadlock:true drawers],log)
		      end
		    else ([],log)
		else 
		  begin
		    let log = Session.add_log_entry 0 "-Outputting stories" log in
		      if !dot_output && (not !html_mode) then
			begin
			  let cpt = ref 0 in
			    Hashtbl.iter (fun _ nets_nb  ->
					    List.iter
					      (fun (net,i,time) -> 
						 let fic_name = 
						   Filename.concat !output_dir  ("h"^(string_of_int (!cpt)^".dot")) 
						 in
						   HTML.dot_of_network fic_name (net,i,(time/.(float_of_int i))) ;
						   incr cpt 
					      ) nets_nb 
					 ) c.drawers.Iso.hsh  
			end;
		      ([Session.xml_of_stories c.drawers],log)
		  end
	      else ([],log)
	    in
	    let data_map = Simulation2.build_data c.concentrations c.time_map sd.obs_ind in
	    let log = Session.add_log_entry 0 ("-Sampling data...") log 
	    in
	    let t0 = Mods2.gettime() in
	    let ls_sim = 
	      if !story_mode or !map_mode then []
	      else [Session.ls_of_simulation sd.rules sd.obs_ind 
		      c.time_map data_map c.curr_step c.curr_time]
	    in
	    let log = 
	      Session.add_log_entry 0 (sprintf "-End of sampling %.4f s CPU" (Mods2.gettime()-.t0)) log 
	    in
	    let log = 
	      if not (!data_file="") or (!html_mode && not !story_mode) then
		begin
		  if !data_file = "" then data_file := HTML.rename "" (!fic) "dat" ;
		  let data_file = Filename.concat !output_dir !data_file in
		  let t0 = Mods2.gettime() in
		  let log = 
		    Session.add_log_entry 0 ("-Outputting simulation data in "^data_file) log 
		  in
		    (*************TEMP DATA RENDERING**************)
		    HTML.print_data data_file sd.rules data_map sd.obs_ind c.time_map ;
		    (**********************************************)
		    Session.output_data data_file sd.rules data_map sd.obs_ind c.time_map ;
		    (*sd.obs_ind c.time_map c.concentrations*) 
		    let log = 
		      Session.add_log_entry 0 (sprintf "-End of data outputting %.4f s CPU" (Mods2.gettime()-.t0)) log 
		    in
		      log
		end
	      else log
	    in
	      (****************HTML RENDERING************)
	      if !html_mode then 
		begin
		  let rules = 
		    if !Config.build_rules then 
		      begin
			fprintf stderr "-Compiling rule page (may take a while)...\n"; flush stderr ;
			HTML.html_of_rules (Filename.concat !output_dir (HTML.rename "rules_" (!fic) "html")) rules ;
			Some (Filename.concat !output_dir (HTML.rename "rules_" (!fic) "html"))
		      end 
		    else None 
		  and compression_log = 
		    if !Data.log_compression then 
		      begin
			fprintf stderr "-Compiling compression log (may take a while)...\n";flush stderr ;
			HTML.html_of_compression_log (Filename.concat !output_dir (HTML.rename "compression_log_" (!fic) "html")) c.compression_log ;
			Some (Filename.concat !output_dir (HTML.rename "compression_log_" (!fic) "html"))
		      end
		    else 
		      None 
		  and influence = 
		    if !build_cause then Some (Filename.concat !output_dir (HTML.rename "influence_map_" (!fic) "dot")) 
		    else None
		  and data = if !story_mode then None else Some (Filename.concat !output_dir !data_file)
		  and final = 
		    if !output_final then Some (HTML.rename "final_" (!fic) "dot")
		    else None
		  and html_file = Filename.concat !output_dir (HTML.rename "" (!fic) "html") 
		  in
		    HTML.build_html html_file influence data rules compression_log final c ;
		end ;
	      (******************************************) 
	      Session.finalize 
		xml_file
		~xml_content:(xml_map,ls_sol,ls_snapshots,xml_stories,ls_sim)
		log
		0
      with 
	  Runtime msg -> let log = Session.add_log_entry 3 msg log in Session.finalize xml_file log 3
	| Runtime2 msg -> let log = Session.add_log_entry 2 msg log in Session.finalize xml_file log 2
	| Sys_error msg -> let log = Session.add_log_entry 2 msg log in Session.finalize xml_file log 2
	| Too_expensive -> 
	    let msg = "Memory limit reached" in
	    let log = Session.add_log_entry 2 msg log in Session.finalize xml_file log 2
	| exn -> let log = Session.add_log_entry 2 (Printexc.to_string exn) log in Session.finalize xml_file log 2

    in 
      if !Mods2.bench_mode then (Gc.print_stat stdout;print_newline ()) else ()
