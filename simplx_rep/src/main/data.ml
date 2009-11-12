(**Data references*)

let version_main = 4
let version_cpt = 21 (*line error in compiler, map mode not parsing init*)
let git_tag = Git_commit_info.git_commit_tag
let version_branch = ""
let arch_type = string_of_int Sys.word_size
let commit_date = Git_commit_info.git_commit_date
let version = (string_of_int version_main)^"."^(string_of_int version_cpt)^version_branch^".."^(string_of_int git_tag)^"_"^arch_type

(**<h3>File naming </h3>*)

(**kappa_file name*)
let fic = ref ""  

(**Data files*)
let data_file = ref ""
let data_desc:out_channel option ref = ref None
let serialized_data_file = ref "~tmp_data"

(**For preventing simplx to run standalone*)
let key = ref "0x0000000000000000"
 
(**name of the xml file containing the results of the current session (default simplx.xml)*)
let xml_session = ref "simplx.xml" 
let skip_xml = ref false 

(**temporary options**)
let html_mode = ref false
let cplx_hsh = ref true
let dot_output = ref false
let no_random_time = ref false

type story_compression_iterator_mode = FIRST | ALL
type story_compression_granularity = WEAK | STRONG

let quotient_refs = ref false (**set true to replace each rule by the most general rule it is a refinement of when computing stories*)
let closure = ref true
let story_compression = ref true
let story_compression_mode = ref FIRST

(*let story_compression_granularity = ref WEAK*)
let story_iteration_strategy = 2 (*1->follow causal order,2->follow linear order*) 
let story_propagation_strategy = 2 (*1->less propagation 2->more propagation *)
let ignore_story_compression = false (*if true, compute compress but ignore the result *)
let max_backtrack = ref 100000 
let max_time_per_compression = ref 60. 
let log_compression = ref false
let log_strong_compression=ref true
let network_display_limit = ref 100
let strong_compression = ref true
let reorder_by_depth = ref true
let use_multiset_in_strong_compression = ref true 
let show_steps_in_compression = ref false
                                 
(**directory to which outputting files (default current dir)*)                        
let output_dir = ref Filename.current_dir_name 

(**directory from which the program is called*)
let base_dir = ref ""                          
                             
(**<h3> serialized file to save or load </h3>*)
let serialized_rule_file = ref "~tmp_rules"
let serialized_mixture_file = ref "~tmp_mixture"
let serialized_sim_data_file = ref "~tmp_sd"
let serialized_snapshot_file = ref "~tmp_snapshot"

let save_rules = ref false
let load_rules = ref false
let load_sim_data = ref false

(**<h3>simulation data</h3>*)

(**Default sample: specifies how many data points per default simulation*)
let data_points = ref 1000 

(**time of computation (default infinite) *)
let max_time = ref (-1.0)  (*negative is infinite*)    

(**size of sample in t.u*)
let time_sample = ref (-1.0) (*negative value means undefined*)

(**size of sample in events (default 100)*)
let step_sample = ref (-1) (*negative value means undefined*)

(**number of storification atempts (default 1)*)
let max_iter = ref 1            

(**number of events during computation (default infinite)*)
let max_step = ref (-1) (*negative is infinite*)

(**time at which to start taking measures/stories (default 0.0)*)
let init_time = ref 0.0         

(**number of clock ticks per run*)
let clock_precision = ref 60

(**max number of successive clashes*)
let max_clashes = ref 10000 (**-1 for infinity by default*)

(**Deadlocked activity threshold (default 0.0)*)
let deadlock_sensitivity = ref 0.0

(**<h3>output info</h3>*)

(**Tick string (default '#')*)
let tick_string = "#"

(**Creates a dot file containing the final state of the simulation (default false)*)
let output_final = ref false

(**Build the negative influence map (default true)*)
let build_conflict = ref true

(**Build the positive influence map (default true) *)
let build_cause = ref true

(**Merges both influence maps (default false)*)
let merge_maps = ref false

(**Creates a gnuplot file (default false)*)
let gnuplot_plugin = ref false

(**<h3> compilation/simulation modes </h3> *)
let forward = ref false

(**storification mode*)
let story_mode = ref false      

(**sample defined in t.u (default true)*)
let time_mode = ref true        

(**compile only mode (default false)*)
let compile_mode = ref false    

(**influence map only mode*)
let map_mode = ref false

(**seed random generator at each new run with given value (default None)*)
let (seed:int option ref) = ref None             

(**rescaling (default 1.0)*)
let rescale = ref 1.0           

let p_intra_fic = ref ""
let plot_p_intra = ref false
let prob_desc:out_channel option ref = ref None

(**ploting*)
let ignore_obs = ref false
let on_the_fly = ref false

(**snapshot mode*)
let snapshot_mode = ref false

(**<h3>talkativity</h3>*)

(**Debug mode (default false)*)
let debug_mode = ref false 
let sanity_check = ref false

(**Garbage collection control*)
let memory_limit = ref 0 
let gc_alarm_high:Gc.alarm option ref = ref None
type gc_mode_t = LOW | HIGH
let gc_mode:gc_mode_t option ref = ref None 
let gc_alarm_low:Gc.alarm option ref = ref None
let gc_overflow:Gc.alarm option ref = ref None
let gc_high = ref 0.9
let gc_low = ref 0.7
let gc_overhead = ref 80 (*ocaml default*)

(**Verbose mode: output all warnings on standard error channel (default false)*)
let verbose = ref false

(**List of temporary files*)
let tmp_file:string list ref = ref []
let cleanup_tmp_file = 
  fun _ -> List.iter (fun file ->
			try
			  Sys.remove file
			with _ -> ()
		     ) !tmp_file

(**<h3>parser results</h3>*)  

(**Compile options*)

let _PARSE_INIT = 1
let _PARSE_RULES = 2
let _PARSE_OBS = 4
let compilation_opt = ref (_PARSE_INIT lor _PARSE_RULES lor _PARSE_OBS) (*7*) 

(**List of rules*)
let (rules:Rule.t list ref) = ref []
let (rule_id:int ref) = ref 0

(**List of observables*)
let (obs_l:Solution.observation list ref) = ref []

(**Initial solution*)
let (init:(Solution.t*int) list ref) = ref [] 

(**Do we take into account coefficients in initial states [JF]*)
let parse_coef = ref true

(**Experiment*)
type instruction = SAVE_MIXTURE | SAVE_STATE | ACTIVATE_PERTURBATION of int | CANCEL_PERTURBATION of int | TAKE_SNAPSHOT 

let tasks:(float * instruction) list ref = ref []
let exp = ref Experiment.empty
let rec is_empty tasks = (tasks = [])

let rec add_task e tasks = 
  let comp (t,tsk) (t',tsk') = 
    if t<t' then (-1) else (if t=t' then 0 else 1)
  in
    match tasks with
	e'::tl -> if comp e e' = (-1) then e::(e'::tl) else e'::(add_task e tl) (*not tail rec!*)
      | [] -> [e]


let max_sol_display = 1000

let env:(string,(Solution.t*int))Hashtbl.t ref = ref (Hashtbl.create 100)


