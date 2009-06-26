open Rule
open Printf
open Error
open Mods2
open Simulation2
open Data
open Key
open Error_handler 

let usage_msg = "SimPlx "^version^": \n"^"Usage is simplx --[sim|compile|cflow] file.ka "
let version_msg = "SIMulator by PLectiX: "^version^"\n"^key_version

let main =
  let options = [ 
    ("--version", Arg.Unit (fun () -> print_string (version_msg^"\n") ; flush stdout ; exit 0), "print simplx version");

    (*Simulation*)
    ("--sim", Arg.String (fun s -> fic := s), "name of the kappa file to simulate");
    ("--time", Arg.Float (fun f -> time_mode:=true ; max_time := f), "(infinite): time units of computation");
    ("--event", Arg.Int (fun i -> time_mode :=false ; max_step := i), "(infinite): number of rule applications");
    ("--points", Arg.Int (fun i -> data_points := i), "number of data points per plots)");
    ("--rescale", Arg.Float (fun f -> rescale := f), "(1.0): rescaling factor (eg. '10.0' or '0.10')") ;
    ("--output-final-state",Arg.Unit (fun () -> output_final:=true),"output final state") ;
    ("--plot",Arg.String (fun s -> on_the_fly:=true ; data_file:=s), "Creates a file containing the simulation data in space separated format");
    ("--deadlock-threshold",
     Arg.Float (fun i -> deadlock_sensitivity:=i),"[expert] Defines the activity of a deadlocked system (default 0.0)");
    ("--gp",Arg.Unit (fun s -> gnuplot_plugin:=true), "Requires the creation of a gnuplot readable file at the end on the simulation");
    ("--compile", Arg.String (fun s -> compile_mode:=true; fic := s), "name of the kappa file to compile");

    (*Causality analysis*)
    ("--cflow", Arg.String (fun s -> story_mode := true ; fic := s), "name of the kappa file to analyse");
    ("--no-compression",Arg.Unit (fun () -> story_compression:=false),"do not compress stories");
    ("--weak-compression",Arg.Unit (fun () -> strong_compression:=false),"use only weak compression to classify stories");
    ("--iteration", Arg.Int (fun i -> 
			       if i>1 then (Data.tasks := Data.add_task (-1.0,Data.SAVE_STATE) !Data.tasks ; max_iter := i)
			       else
				 if i<0 then (Printf.fprintf stderr "Iteration should be greater than 0!\n" ; exit 1)
				 else
				   max_iter := i), 
     "(1): number of stories to be searched for (with --storify option only)");
    ("--init", Arg.Float (fun f -> init_time := f), "(0.0): start taking measures (stories) at indicated time");
    ("--no-arrow-closure",Arg.Unit (fun () -> closure:=false),
     "do not perform arrows transitive closure when displaying stories");
    ("--quotient-refinements", Arg.Unit (fun () -> quotient_refs:=true), "replace each rule by the most general rule it is a refinement of when computing stories");
    ("--dot-output",Arg.Unit (fun () -> dot_output:=true), "dot output for stories") ;

    ("--generate-map", Arg.String (fun s -> map_mode :=true ; fic := s), "name of the kappa file for which the influence map should be computed");
    ("--no-inhibition-map",Arg.Unit (fun () -> build_conflict:=false),"do not construct inhibition map");
    ("--no-activation-map",Arg.Unit (fun () -> build_cause:=false),"do not construct activation map");
    ("--no-maps",Arg.Unit (fun () -> build_conflict:=false ; build_cause:=false),
     "do not construct inhibition/activation maps");
    ("--merge-maps",Arg.Unit (fun () -> merge_maps:=true),"also constructs inhibition maps");

    ("--output-dir", Arg.String (fun s -> output_dir := Filename.concat (Filename.current_dir_name) s), 
     "(current dir) directory on which to put computed data"); 
    ("--seed",Arg.Int (fun i -> seed:=Some i), "seed the random generator using given integer (same integer will generate the same random number sequence)");
    ("--no-measure", Arg.Unit (fun () -> ignore_obs:=true), "causes simplx to ignore observables") ;
    ("--memory-limit",Arg.Int (fun i -> memory_limit:=i), "limit the usage of the memory (in Mb). Default is infinite (0)");
    
    (*Tasks*)
    ("--snapshot-at", Arg.Float (fun f -> snapshot_mode:=true ; Data.tasks := Data.add_task (f,Data.TAKE_SNAPSHOT) !Data.tasks),
     "takes a snapshot of solution at specified time unit (may use option several times)");
    ("--mixture-file-scheme" , Arg.String (fun s -> serialized_mixture_file := s), "(~tmp_mixture_[t]) Naming scheme for serialization of mixtures") ;
    ("--save-mixture-at", Arg.Float (fun f -> Data.tasks := Data.add_task (f,Data.SAVE_MIXTURE) !Data.tasks), 
     "Save mixture at specified time (can be used multiple times)") ;
    ("--load-mixture", Arg.String (fun s -> compilation_opt:= !compilation_opt land (lnot _PARSE_INIT) ; serialized_mixture_file :=s), 
     "Use given mixture file as initial conditions (%init instructions in the kappa file will be ignored)") ;
    ("--state-file-scheme" , Arg.String (fun s -> serialized_sim_data_file := s), "(~tmp_state_[t]) Naming scheme for serialization of simulation state") ;
    ("--save-state-at", Arg.Float (fun f -> Data.tasks := Data.add_task (f,Data.SAVE_STATE) !Data.tasks),
     "Save simulation state at specified time (can be used multiple times)");
    ("--load-state", Arg.String (fun s -> compilation_opt := 0; load_sim_data:=true ; serialized_sim_data_file := s), 
     "Load given simulation state (only %mod instruction will be parsed from the kappa file)");
    ("--max-clashes", Arg.Int (fun i -> max_clashes := i), "(10000) max number of consecutive clashes before aborting"); 
    (*expert mode options*)
    
    ("--key", Arg.String (fun s -> key := s), "[expert] name of the file containing the key for the crypted version");
    ("--clock-precision",Arg.Int (fun i -> clock_precision:=i) , "[expert] (60) clock precision (number of ticks per run)");
    ("--debug",Arg.Unit (fun () -> debug_mode:=true), "[expert] debug mode (very verbose!)");
    ("--QA",Arg.Unit (fun () -> sanity_check:=true), "[expert] QA mode (slower, but performs more sanity checks)");
    ("--profile",Arg.Unit (fun () -> Mods2.bench_mode:=true), "[expert] to produce profiling.");
    ("--gc-high"),Arg.Int (fun i -> if i>100 then gc_high:=1.0 else gc_high:=(float_of_int i)/.100.) , 
    "[expert] (90) triggers strong garbage collection when memory usage is above the given percentage of memory limit" ;
    ("--gc-low"),Arg.Int (fun i -> if i>100 then gc_low:=1.0 else gc_low:=(float_of_int i)/.100.) , 
    "[expert] (70) faster garbage collection when memory usage is below the given percentage of memory limit" ;
    ("--set-gc-overhead",Arg.Int (fun i -> gc_overhead:=i),
     "[expert] (80) tune the gc speed. Value below 80 will result in better gc but poorer performances") ;
    ("--xml-session-name",Arg.String (fun s -> xml_session:=s),
     "[expert] name of the xml file containing results of the current session (default simplx.xml)");
    ("--plot-prob-intra", Arg.String (fun s -> plot_p_intra:=true ; p_intra_fic:=s), "[expert] Plot the evolution of the proba of intra during time in given file name");
    ("--snapshot-tmp-file", Arg.String (fun s -> serialized_snapshot_file:=s), 
     "[expert] set name for the temporary snapshots file (default ~tmp_snapshots)");
    
    (*temporary options*)
    ("--storify", Arg.String (fun s -> story_mode := true ; fic := s), "[deprecated]");
    ("--set-snapshot-time", Arg.Float (fun f -> snapshot_mode:=true ; Data.tasks := Data.add_task (f,Data.TAKE_SNAPSHOT) !Data.tasks),
     "[deprecated]");
    ("--data-tmp-file", Arg.String (fun s -> serialized_data_file:=s), 
     "[deprecated]");
    ("--no-compress-stories",Arg.Unit (fun () -> story_compression:=false),"[deprecated]");
    ("--no-use-strong-compression",Arg.Unit (fun () -> strong_compression:=false),
     "[deprecated] as --no-strong-compression (for backward compatibility)");
    ("--light-xml", Arg.Unit (fun () -> skip_xml:=true), "[temporary] prevent simplx from building xml structures for results") ;
    ("--no-seed",Arg.Unit (fun () -> seed:=Some 0),"[temporary] equivalent to --seed 0. Kept for compatibilty issue") ; 
    ("--compress-stories",Arg.Unit (fun () -> story_compression:=true),"[temporary] compression of stories");
    ("--use-strong-compression",Arg.Unit (fun () -> strong_compression:=true),
     "[temporary] use strong compression to classify stories");
    ("--forward",Arg.Unit (fun () -> forward:=true), "[temporary] do not consider backward rules" );
    ("--show-steps-in-compression",Arg.Unit (fun () -> show_steps_in_compression:=true),
     "[temporary] display all step of story compressions in the standard output");
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
    if not (good_key !key) then (print_string "Invalid licence key\n" ; flush stdout ; exit 1) ;
    
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

    (*Redefining CTRL+C for cleaning up temp files*)
    let sigint_handle = fun _ ->
      raise Error.Interrupted 
    in
    let _ = Sys.set_signal Sys.sigint (Sys.Signal_handle sigint_handle) in
      
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
	  let log,t_compil = (Session.add_log_entry 0 "-Compilation..." log, Mods2.gettime()) 
	  in
	    (*compute rules,sol_init,obs_l and exp from a kappa file*)
	    try
	      let (r,i,o,e) = Kappa_lex.compile (!fic) in
	      let s = Solution.sol_of_init !compile_mode i in
	      let log = 
		Session.add_log_entry 0 (sprintf "-Compilation: %f sec. CPU" (Mods2.gettime()-.t_compil)) log 
	      in
		(log,0,r,i,s,o,e)
	    with
		Error.Syntax (msg,line) -> 
		  let log = Session.add_log_entry 2 msg ~line:line log in
		    (log,2,[],[],Solution.empty(),[],Experiment.empty)
	in
	  if (warn > 1) or (!compile_mode) then 
	    begin
	      (************* TEMP RENDERING OF RULES***************)
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
	    if !story_mode then  (*checking story observable was defined*)
	      if not (List.exists (fun obs -> match obs with Solution.Story _ -> true | _ -> false) obs_l)
	      then 
		let log = Session.add_log_entry 2 ("No story observation defined in "^(!fic)^". Aborting") log in
		  Session.finalize xml_file log 2
	      else () ;

	  (*Seeding random number generator*)
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

	  (***************INITIALIZATION*****************************)
	  let t_init = Mods2.gettime() in
	  let log = Session.add_log_entry 0 "-Initialization..." log in
	  let log,p,sd,c = 
	    
	    let log = Session.add_log_entry 0 "--Computing initial state" log 
	    in
	      Simulation2.init log (rules,init,sol_init,obs_l,exp)  
	  in
	  let sd = 
	    if !story_mode then (*adding constraints to stories*)
	      List.fold_left (fun sd obs ->
				match obs with
				    Solution.Story (set,flg) ->
				      begin
					try
					  let i = StringMap.find flg sd.rule_of_name in
					  let r,a = Rule_of_int.find i sd.rules in
					  let rids = StringSet.fold (fun flg set -> 
								       let i = StringMap.find flg sd.rule_of_name in
									 IntSet.add i set
								    ) set IntSet.empty 
					  in
					  let r = {r with constraints = (ROOTED_STORY rids)::r.constraints} in
					    {sd with rules = Rule_of_int.add i (r,a) sd.rules}
					with Not_found -> failwith "Main: flag doesn't correspond to any rule"
				      end
				  | _ -> sd
			     ) sd obs_l
	    else sd
	  in
	  let msg = sprintf "-Initialization: %f sec. CPU" (Mods2.gettime() -. t_init) in
	  let log = Session.add_log_entry 0 msg log in
	  let _ = Gc.compact() in (*collecting dead memory at the end of initialization*)
	    (*end initialization*)
	  let log,sd,p,c = 
	    if not !map_mode then
	      begin
		let log = Session.add_log_entry 0 (sprintf "-Simulation (t=%f)..."c.curr_time) log in
		let t_sim = Mods2.gettime() in
		let task_list = 
		  IntMap.fold (fun i t task_list -> 
				 let t = if t<0. then c.t0 else t in
				   Data.add_task (t,Data.ACTIVATE_PERTURBATION i) task_list
			      ) sd.lab.Experiment.time_on !Data.tasks 
		in
		let task_list = 
		  IntMap.fold (fun i t task_list ->
				 if t>0. then Data.add_task (t,Data.CANCEL_PERTURBATION i) task_list
				 else task_list 
			      ) sd.lab.Experiment.time_off task_list
		in
		  
		let rec print_bar n =
		  if n=0 then (print_newline() ; flush stdout) 
		  else
		    (print_string "_" ; print_bar (n-1))
		in
		let _ = print_bar !Data.clock_precision in
		  
		let _ = data_desc := if !Data.on_the_fly then Some (open_out (Filename.concat !output_dir !data_file)) else None in
		  
		(*************************************************************************************************)
		(**************************************begin loop function****************************************)
		(*************************************************************************************************)
		let rec loop log sd p c = 
		  (*Time course*)
		  let c = Time_course.output_data_point data_desc sd p c in

		  (*Progress bar*)
		  let c = ticking c in
		    
		  (*Stop conditions*)
		  let log,sd,p,c,stop =
		    if c.restart then
		      if c.curr_iteration >= !max_iter then
			begin (*exiting event loop*)
			  Printf.printf "\n"; flush stdout ;
			  let log = Session.add_log_entry 0 (Printf.sprintf "-Exiting storification after %d iteration(s)" c.curr_iteration) log in
			    (log,sd,p,{c with 
					 curr_tick = 0 ;
					 curr_time = 0.0 ;
					 curr_step = 0 ;
					 restart = false
				      },true)
			end
		      else (*not the last iteration*)
			let init_sd,c,log = 
			  match p.init_sd with
			      None -> Error.runtime (None,None,None) "Cannot find marshalized simulation data file!"
			    | Some serialized_sim_data ->
				let log = Session.add_log_entry 4 ("-Loading initial state from "^serialized_sim_data^"...") log in
				  compilation_opt := 0 ; load_sim_data := true;
				  let _,_,_,exp = Kappa_lex.compile !fic in
				  let task_list = 
				    IntMap.fold (fun i t task_list -> 
						   let t = if t<0. then c.t0 else t in
						     Data.add_task (t,Data.ACTIVATE_PERTURBATION i) task_list
						) exp.Experiment.time_on []
				  in
				  let task_list = 
				    IntMap.fold (fun i t task_list ->
						   if t>0. then Data.add_task (t,Data.CANCEL_PERTURBATION i) task_list
						   else task_list 
						) exp.Experiment.time_off task_list
				  in
				  let d = open_in_bin serialized_sim_data in 
				  let _,f_init_sd = (Marshal.from_channel d : float * marshalized_sim_data_t) in
				  let _ = close_in d in
				  let sd = unmarshal f_init_sd in
				    ({sd with lab = exp ; task_list = task_list},{c with 
										    curr_tick = 0 ;
										    curr_time = 0.0 ;
										    curr_step = 0 ;
										    restart = false
										 },log)
			in
			  Gc.compact() ;
			  (log,init_sd,p,c,false)
		    else (*no restart needed*)
		      if c.deadlock && Data.is_empty task_list then 
			begin
			  Printf.printf "\n"; flush stdout ;
			  let log = Session.add_log_entry 1 (Printf.sprintf "-Stalled system at time %f (after %d events)..." c.curr_time c.curr_step) log
			  in
			    (log,sd,p,c,true)
			end
		      else
			if (!max_time >= 0.0) && (c.curr_time > !max_time) or ((!max_step >= 0) && (c.curr_step > !max_step)) then (*time or event limit reached*)
			  begin (*exiting event loop*)
			    Printf.printf "\n"; flush stdout ;
			    let log = Session.add_log_entry 0 (Printf.sprintf "-Exiting simulation at time %f (after %d events)" c.curr_time c.curr_step) log in
			      (log,sd,p,c,true)
			  end
			else (*time or event limit not reached*)
			  (log,sd,p,c,false)
		  in
		    if stop then 
		      let sd,c,p,log = Monitor.apply sd c p log true in
			(log,sd,p,c)
		    else
		      
		      (*loop should go on!*)
		      let sd,c,p,log = Monitor.apply sd c p log false
		      in
		      let p,log =
			match !gc_mode with
			    Some HIGH -> 
			      if p.gc_alarm_high then (p,log) 
			      else 
				let log = Session.add_log_entry 1 "Free memory is low, shifting to strong garbage collection" log in
				  ({p with gc_alarm_high=true ; gc_alarm_low=false},log)
			  | Some LOW ->
			      if p.gc_alarm_low then (p,log) 
			      else 
				let log = Session.add_log_entry 4 "Using low garbage collection" log in
				  ({p with gc_alarm_high=false ; gc_alarm_low=true},log)
			  | None -> (p,log)
		      in
			
		      let _ = if !debug_mode then print sd else () in

		      let sd,p,story_mode =
			if !story_mode && (!init_time <= c.curr_time) && (Network.is_empty sd.net) then 
			  (
			    if !debug_mode then Printf.printf "Initializing network\n" ; flush stdout ;
			    let net = init_net Network.empty sd.sol in
			    let sd = {sd with net = net} in
			      (sd,p,true)
			  )
			else
			  (sd,p,!story_mode && (!init_time <= c.curr_time))
		      in
			
		      let (log,sd,p,c) = event log sd p c story_mode in
			loop log sd p c 
		in
		  (*************************************************************************************************)
		  (**************************************end loop function******************************************)
		  (*************************************************************************************************)
		  
		let log,sd,p,c = loop log {sd with task_list = task_list} p c in
		  (*
		    let log,drawers,compress_log  = 
		    Iso.compress_drawers log c.drawers p.iso_mode (fun a b c -> Session.add_log_entry a b c) 
		    in 		 
		    let c = 
		    {c with drawers = drawers ;
		    compression_log = compress_log } in 
		  *)
		  (Session.add_log_entry 0 (sprintf "-Simulation: %f sec. CPU" (Mods2.gettime() -. t_sim)) log,sd,p,c)
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
		      ([Session.xml_of_maps sd.rules ~conflict:sd.conflict sd.flow],log)
		    else 
		      ([Session.xml_of_maps sd.rules sd.flow],log)
		end
	      else ([],log)
	    in
	    let ls_sol,log = 
	      if !output_final then 
		let log = Session.add_log_entry 0 "-Outputting final state as required" log in
		let species = Species.of_sol sd.sol in
		let (ls_head,ls_core,ls_tail) = Session.ls_of_species species c.curr_time in
		  ([(ls_head,ls_core,ls_tail)],log)
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
		    gather_snapshots !Data.serialized_snapshot_file (c.snapshot_counter-1) ([],log)
		end
	      else ([],log)
	    in
	    let xml_stories,log = 
	      if !story_mode then 
		if Iso.nb_stories c.drawers = 0 then
		  let log = Session.add_log_entry 1 "-No causal trace was found withing given time or event limits!" log in ([],log)
		else
		  begin
		    let log = Session.add_log_entry 0 (Printf.sprintf "-found %d causal trace(s), now outputting..." (Iso.nb_stories c.drawers)) log in
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
	    let filtered_obs_ind = IntSet.filter (fun i -> 
						    try
						      let r,_ = Rule.Rule_of_int.find i sd.rules in
							not (r.Rule.input = "var")
						    with
							Not_found -> 
							  Error.runtime (None,None,None) ("Cannot find obs "^(string_of_int i))
						 ) sd.obs_ind
	    in
	    let data_map = Simulation2.build_data c.concentrations c.time_map filtered_obs_ind
	    in
	    let log = Session.add_log_entry 0 ("-Sampling data...") log 
	    in
	    let t0 = Mods2.gettime() in
	    let ls_sim = 
	      if !story_mode or !map_mode then []
	      else [Session.ls_of_simulation sd.rules filtered_obs_ind
		      c.time_map data_map c.curr_step c.curr_time]
	    in
	    let log = 
	      Session.add_log_entry 0 (sprintf "-End of sampling %.4f s CPU" (Mods2.gettime()-.t0)) log 
	    in
	    let _ = 
	      let _ = 
		match !data_desc with
		    Some d -> close_out d
		  | None -> ()
	      in
		if !Data.gnuplot_plugin then 
		  Time_course.make_gnuplot_file (Filename.concat !output_dir !data_file) sd 
		else ()
	    in
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
	| Interrupted ->
	    let msg = "Simulation interrupted by user" in
	    let log = Session.add_log_entry 1 msg log in Session.finalize xml_file log Sys.sigint
	| exn -> let log = Session.add_log_entry 2 (Printexc.to_string exn) log in Session.finalize xml_file log 2
    in 
      if !Mods2.bench_mode then (Gc.print_stat stdout;print_newline ()) else ()
