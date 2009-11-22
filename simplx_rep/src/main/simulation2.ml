open Mods2
open Error			
open Rule
open Data
open Experiment
open Error_handler_common
open Error_handler 


module InjArray = Array_ext.Make(struct 
				   type t = (int list)*int*(int AssocArray.t) 
				   let default = ([],0,AssocArray.create 5) 
				 end)


type sim_data_type = {(*fresh_ind:int ;*)
  rules: Rule_of_int.t ;
  rule_of_name: int StringMap.t ;
  obs_ind : IntSet.t;

  (*coord[ind_r;ind_cc] -> (free_keys,fresh_key,assoc_map)*)
  injections: Coord.t InjArray.t;

  (*(#i,s)->{...;(coord_i[i_r,i_cc],i_inj);...}*)
  lift:(int*string) CoordSetArray.t ;

  flow:IntSet.t IntMap.t ;
  conflict: IntSet.t IntMap.t;
  init: (Solution.t * int) list ;
  sol:Solution.t ;
  net:Network.t ;
  n_ag:int ;
  min_rate:float ;
  lab: Experiment.t ;
  inf_list: IntSet.t; (*indices of rules with current infinite rate*)
  oo: IntSet.t ; (*rules which have to be put to inf_list when instances>0*)
  incompressible: IntSet.t ;(*rule ids that should not be compressed when storifying*)
  task_list: (float*instruction) list 
}

(*Marshalization of sim_data*)
type marshalized_sim_data_t = {
  f_rules: (int*(Rule.marshalized_t*float)) list ;
  f_rule_of_name: int StringMap.t ;
  f_obs_ind : IntSet.t;
  (*[ind_r;ind_cc] -> (free_keys,fresh_key,assoc_map)*)
  f_injections: (Coord.t (*key*) * (*--->*) (int list (*fresh_keys*) 
					     * int (*fresh_key*) 
					     * ((int*assoc) list) (*f_assoc list*)
					    )
		) list; 
  (*(#i,s)->{[i_r,i_cc,i_inj];...;[j_r,j_cc,j_ind]}*)
  f_lift:((int*string)*((Coord.t*int) list)) list ; 
  f_flow:IntSet.t IntMap.t ;
  f_conflict: IntSet.t IntMap.t;
  f_init: (Solution.marshalized_t * int) list ;
  f_sol:Solution.marshalized_t ;
  f_net:Network.marshalized_t ;
  f_n_ag:int ;
  f_min_rate:float ;
  f_inf_list: IntSet.t;
  f_oo: IntSet.t ;
  f_incompressible : IntSet.t
}

let marshal sim_data = 
  {
    f_rules = Rule_of_int.fold (fun i (r,act) cont -> (i,(Rule.marshal r,act))::cont) sim_data.rules [] ;
    f_rule_of_name = sim_data.rule_of_name ;
    f_obs_ind = sim_data.obs_ind ;
    f_injections = InjArray.fold (fun coord (fresh_keys,fresh,assoc_array) cont ->
				    let f_assoc_list = 
				      AssocArray.fold (fun assoc_id assoc cont ->
							 (assoc_id,assoc)::cont
						      ) assoc_array []
				    in
				      (coord,(fresh_keys,fresh,f_assoc_list))::cont
				 ) sim_data.injections [] ;

    f_lift = CoordSetArray.fold (fun (i,s) coordSet cont -> 
				   let coordList =
				     CoordSet.fold (fun (coord_i,i_inj) cont -> (coord_i,i_inj)::cont) coordSet []
				   in
				     ((i,s),coordList)::cont
				) sim_data.lift [] ;
    f_flow = sim_data.flow ;
    f_conflict = sim_data.conflict ;
    f_init = List.map (fun (s,i) -> (Solution.marshal s,i)) sim_data.init ;
    f_sol = Solution.marshal sim_data.sol ; 
    f_net = Network.marshal sim_data.net ; 
    f_n_ag = sim_data.n_ag ; 
    f_min_rate = sim_data.min_rate ; 
    f_inf_list = sim_data.inf_list ;
    f_oo = sim_data.oo ;
    f_incompressible = sim_data.incompressible 
  }

let unmarshal f_sd = 
  try
    {
      rules =
	begin
	  let size = List.length f_sd.f_rules in
	    List.fold_left (fun rule_of_int (i,(f_r,act)) -> 
			      Rule_of_int.add i (Rule.unmarshal f_r,act) rule_of_int
			   ) (Rule_of_int.empty size) f_sd.f_rules 
	end;
      rule_of_name = f_sd.f_rule_of_name ;
      obs_ind = f_sd.f_obs_ind ;
      injections = List.fold_left (fun injs (coord,(fresh_keys,fresh,f_assoc_list)) ->
				     let assocArray = 
				       List.fold_left (fun assoc_array (assoc_id,assoc) ->
							 AssocArray.add assoc_id assoc assoc_array
						      ) (AssocArray.create 1) f_assoc_list 
				     in
				       InjArray.add coord (fresh_keys,fresh,assocArray) injs
				  ) (InjArray.create 1) f_sd.f_injections ;
      
      lift = List.fold_left (fun lift ((i,s),coordList) -> 
			       let coordSet = 
				 List.fold_left (fun set (coord_i,i_inj) ->
						   CoordSet.add (coord_i,i_inj) set
						) CoordSet.empty coordList
			       in
				 CoordSetArray.add (i,s) coordSet lift
			    ) (CoordSetArray.create 1) f_sd.f_lift ;
      flow = f_sd.f_flow ;
      conflict = f_sd.f_conflict ;
      init = List.map (fun (f_s,i) -> (Solution.unmarshal f_s,i)) f_sd.f_init ;
      sol = Solution.unmarshal f_sd.f_sol ; 
      net = Network.unmarshal f_sd.f_net ; 
      n_ag = f_sd.f_n_ag ; 
      min_rate = f_sd.f_min_rate ; 
      lab = Experiment.empty ;
      inf_list = f_sd.f_inf_list ;
      oo = f_sd.f_oo ;
      incompressible = f_sd.f_incompressible ;
      task_list = []
    }
  with _ -> 
    let s = "Simulation.unmarshal: uncaught exception" in
    runtime
      (Some "simulation2.ml",
       Some 140,
       Some s)
      s

let solution_AA_create = ((Solution.AA.create 5:Agent.t Solution.AA.t))

let sd_empty() = {
  rules = Rule_of_int.empty 1 ;
  rule_of_name = StringMap.empty ;
  obs_ind = IntSet.empty;
  lift = CoordSetArray.create 100 ; (*imp*)
  injections = InjArray.create 100 ;(*imp*)
  flow = IntMap.empty ;
  conflict = IntMap.empty;
  init = [] ;
  sol = Solution.empty() ;          (*imp*)
  net = Network.empty();            (*imp*)
  n_ag = 0 ;
  min_rate = (-1.0) ;
  lab = Experiment.empty ;
  inf_list = IntSet.empty ;
  oo = IntSet.empty ;
  incompressible = IntSet.empty ;
  task_list = []
}

type sim_parameters = {
  (*Name of the file containing the serialized sim data*)
  init_sd:string option ;

  (*Computation limits*)
  max_failure:int ; (*max number of failure when trying to apply rules*)
  
  (*Computation modes*)
  compress_mode:bool;
  iso_mode:bool ;
  gc_alarm_high:bool ;
  gc_alarm_low:bool ;
}

type interval = DE of int | Dt of float | Undef

type sim_counters = {curr_iteration:int;
		     curr_step:int;
		     t0:float; (*initial time of the simulation*)
		     curr_time:float;
		     curr_tick:int;
		     skipped:int;
		     compression_log: (Network.t * Network.t * Network.t * int * float) list;
		     drawers:Iso.drawers ;
		     measure_interval: interval ; 
		     points: (int * float * (string list)) list ; (*(0,t_0,<obs_1,...,obs_n>)*....(k,t_k,<obs_1,...,obs_n>)*)
		     last_k: int ;
		     clock_precision : int ;
		     snapshot_counter : int ;
		     deadlock : bool ;
		     restart: bool 
		    }

let empty_counters = {curr_iteration=0;
		      curr_step=0;
		      curr_tick = 0;
		      t0 = 0.0;
		      curr_time=0.0;
		      skipped=0;
		      drawers=Iso.empty_drawers 0;
		      compression_log=[];
		      measure_interval = Undef ;
		      points = [] ;
		      last_k = 0 ;
		      clock_precision=0 ;
		      snapshot_counter = 0 ;
		      deadlock = false ;
		      restart = false 
		     }

let print_injections only_obs sim_data =
  InjArray.iter (fun coord (_,_,assoc_map) ->
		   let (i,j) = Coord.to_pair coord in
		     if not only_obs or (IntSet.mem i sim_data.obs_ind) then 
		       AssocArray.iter (fun i assoc ->
					  let str = string_of_map string_of_int string_of_int IntMap.fold assoc in
					    Printf.printf "CC[%s]:%s_%d\n" (Coord.to_string coord) str i
				       ) assoc_map ;
		     print_string "-----\n" 
		) sim_data.injections


let print_lift lift =
  (*if PortMap.is_empty lift then Printf.printf "empty\n" *)
  if CoordSetArray.is_empty lift then Printf.printf "empty\n" 
  else
    (* PortMap.iter (fun (i,s) set -> *)
    CoordSetArray.iter (fun (i,s) set -> 
			 let str = string_of_set string_of_coord CoordSet.fold set in
			   Printf.printf "(%d,%s)->%s\n" i s str
		      ) lift

let print_rules rules inf_list = 
  Rule_of_int.iter (fun i (r,inst) ->
		      let auto = match r.automorphisms with 
			  None -> (failwith "automorphisms not computed!") 
			| Some i -> if not (i=1) then (Printf.sprintf "/%d" i) else ""
		      in
		      let act = r.kinetics *. inst in
			if r.input = "obs" then
			  match r.flag with
			      Some flg ->
				(Printf.printf "obs[%d]: %s %f%s\n" i flg act auto; flush stdout)
			    | None -> Error.warning (Printf.sprintf "Simulation.print_rules: observation r[%d] has no flag nor input string" i) 
			else
			  if r.input = "var" then
			    match r.flag with
				Some flg ->
				  (Printf.printf "var[%d]: %s %f%s\n" i flg act auto; flush stdout)
			      | None -> Error.warning (Printf.sprintf "Simulation.print_rules: variable r[%d] has no flag nor input string" i) 
			  else
			    if IntSet.mem i inf_list then
			      Printf.printf "r[%d]: %s $INF\n" i r.input
			    else
			      Printf.printf "r[%d]: %s %f%s\n" i r.input act auto; 
			flush stdout ;
		   ) rules


let print sim_data =
  let only_obs = if sim_data.n_ag < Data.max_sol_display then false else true
  in
    if not only_obs then
      Printf.printf "Current solution: %s\n" (Solution.kappa_of_solution ~full:true sim_data.sol);
    print_rules sim_data.rules sim_data.inf_list ;
    print_injections only_obs sim_data
    (*if not only_obs then print_lift sim_data.lift *)

let init_counters init_time sim_data = 
  {curr_iteration = 0; 
   curr_step = 0 ;
   curr_tick = 0 ;
   t0 = init_time ;
   curr_time = init_time;
   skipped = 0;
   drawers = Iso.empty_drawers !max_iter ;
   compression_log = [] ; 
   measure_interval = if !time_mode then (Dt !time_sample) else (DE !step_sample) ;
   points = [] ;
   last_k = 0 ;
   clock_precision = !clock_precision ;
   snapshot_counter = 0 ;
   deadlock = false ;
   restart = false
  }

let add_rule is_obs r sim_data = 
  let sol_init = sim_data.sol
  and indice_r = r.id 
  in
  let is_fake = (r.input = "var") or (r.input = "obs") in
  let obs_ind = if is_obs (*rule activity observed*) or is_fake (*number of agents*) then IntSet.add indice_r sim_data.obs_ind else sim_data.obs_ind
  in
  let injs,lift,instances = 
    IntMap.fold 
      (fun indice_cc lhs_i (injs,lift,instances) -> 
	 try
	   let precomp = IntMap.find indice_cc r.precompil in
	     (*if warn one is not sure at least a modified quark in used by the awaken rule*)
	   let assoc_map = 
	     try
	       Solution.unify ~rooted:true (precomp,lhs_i) (sol_init.Solution.agents,sol_init) 
	     with Not_found -> AssocArray.create 0
	   in
	   let length = AssocArray.size assoc_map (*IntMap.size assoc_map*) in
	   let coord_cc = Coord.of_pair (indice_r,indice_cc) in (*coordinate of lhs_i is (indice_r,indice_cc)*)
	   let injs = InjArray.add coord_cc ([],length,assoc_map) injs in
	   let instances = (float_of_int length) *. instances in 
	   let lift =
	     (*IntMap.fold*)
	     AssocArray.fold
	       (fun indice_assoc assoc_i lift ->
		  let coord_assoc = (Coord.of_pair (indice_r,indice_cc),indice_assoc) in (*coordinate of injection*)
		  let phi id_p =
		    try IntMap.find id_p assoc_i 
		    with Not_found -> 
		      let s = "Simulation.add_rule: assoc invariant violation" in
		      runtime
			(Some "simulation2.ml",
			 Some 319,
			 Some s)
			s
		  and lnk s = s^"!"
		  and inf s = s^"~"
		  in
		    Solution.AA.fold (fun id_pat ag_pat lift ->
					let id_sol = phi id_pat in
					  Agent.fold_interface 
					    (fun site (info,link) lift -> (*we add all quarks of ag_pat*)
					       let quark1 = (id_sol,lnk site)
					       and quark2 = (id_sol,inf site)
					       in 
					       let set1 = 
						 try (*PortMap.find quark1 lift*)
						   CoordSetArray.find quark1 lift

						 with Not_found -> CoordSet.empty
					       and set2 =
						 try (*PortMap.find quark2 lift*)
						   CoordSetArray.find quark2 lift
						 with Not_found -> CoordSet.empty
					       in
					       let lift =
						 if link = Agent.Wildcard then lift 
						   else 
						     let set1 = CoordSet.add coord_assoc set1 in
						       (*PortMap.add quark1 set1 lift*)
						       CoordSetArray.add quark1 set1 lift
					       in
					       if info = Agent.Wildcard then lift 
						 else
						   (*PortMap.add quark2 (CoordSet.add coord_assoc set2) lift*)
						   CoordSetArray.add quark2 (CoordSet.add coord_assoc set2) lift
					    ) ag_pat  lift
				     ) lhs_i.Solution.agents lift
	       ) assoc_map lift
	   in
	     (injs,lift,instances)
	 with Solution.Matching_failed -> (injs,lift,0.0)
      ) (r.lhs) (sim_data.injections,sim_data.lift,1.0) 
  in
    
  (*computing flow and conflict map, not efficient*)
  (*(<<) is the activation relation defined in module Rule*)
  (*(%>) is the inhibition relation defined in module Rule*)
  let flow,conflict = 
    if !cplx_hsh then (IntMap.empty,IntMap.empty)
    else
      let flow = 
	let set = try IntMap.find indice_r sim_data.flow with Not_found -> IntSet.empty in
	  if r << r then IntMap.add indice_r (IntSet.add indice_r set) sim_data.flow else sim_data.flow
      in
	Rule_of_int.fold (fun ind' (r',_) (flow,conflict) -> 
			    if indice_r = ind' then (flow,conflict) 
			    else
			      let r',_ = Rule_of_int.find ind' sim_data.rules in
			      let flow = 
				let set = try IntMap.find indice_r flow with Not_found -> IntSet.empty in
				  if r << r' then IntMap.add indice_r (IntSet.add ind' set) flow else flow
			      in 
			      let flow = 
				let set = try IntMap.find ind' flow with Not_found -> IntSet.empty in
				  if r' << r then IntMap.add ind' (IntSet.add indice_r set) flow else flow
			      in
			      let conflict = 
				if !build_conflict then
				  let conflict = 
				    let set = try IntMap.find indice_r conflict with Not_found -> IntSet.empty in
				      if r %> r' then 
					IntMap.add indice_r (IntSet.add ind' set) conflict 
				      else conflict
				  in
				  let set = try IntMap.find ind' conflict with Not_found -> IntSet.empty in
				    if r' %> r then 
				      begin
					(*
					  Printf.printf "%s #> %s\n" (Rule.string_of_rule r')(Rule.string_of_rule r) ;
					  flush stdout ;
					*)
					IntMap.add ind' (IntSet.add indice_r set) conflict 
				      end
				    else conflict
				else
				  conflict
			      in
				(flow,conflict)
			 ) sim_data.rules (flow,sim_data.conflict)
  in
  let k_r = r.kinetics in
  let inst_r = instances in
  let rn,wrn = 
    if StringMap.mem (Rule.name r) sim_data.rule_of_name then (
      Error.warning (Printf.sprintf "Kappa file uses rule name %s multiple times" (Rule.name r)) ;
      (StringMap.add r.input indice_r sim_data.rule_of_name,true)
    )
    else
      (StringMap.add (Rule.name r) indice_r sim_data.rule_of_name,false)
  in
  let r = if wrn then {r with id = indice_r ; flag = None} else {r with id = indice_r} in
    {sim_data with
       rules = Rule_of_int.add indice_r (r,inst_r) sim_data.rules;
       rule_of_name = rn ;
       injections = injs ;
       lift = lift ;
       flow = flow ;
       conflict = conflict ;
       obs_ind = obs_ind ;
       min_rate = 
	if (sim_data.min_rate < 0.0) && not is_fake then k_r
	else 
	  if not is_fake && (k_r < sim_data.min_rate) then k_r
	  else sim_data.min_rate ;
       inf_list = if r.infinite && (int_of_float instances > 0) then IntSet.add indice_r sim_data.inf_list else sim_data.inf_list ;
       oo = if r.infinite then IntSet.add indice_r sim_data.oo else sim_data.oo
    }

let init_net net (sol_init:Solution.t) = (*linear in the size of sol_init*)
  let l_cc,_ = 
    Solution.AA.fold (fun i _ (cont,blacklist) -> 
			if IntSet.mem i blacklist then (cont,blacklist)
			else
			  let cc_i,blacklist = (*Solution.connected_component i sol_init*) Solution.cc_of_id i sol_init blacklist 
			  in
			    (cc_i::cont,(*IntSet.union cc_i*) blacklist)
		     ) sol_init.Solution.agents ([],IntSet.empty)
  in
  let rec aux l_cc net = 
    match l_cc with 
	[] -> net
      | cc_i::tl -> 
	  let modifs = 
	    Solution.AA.fold (fun i ag modif ->
				Agent.fold_interface 
				  (fun s _ pmap -> 
				     let (inf,lnk) = Agent.state ag s in
				     let state_inf = match inf with
					 Agent.Marked mk -> Rule.Init_mark (Agent.name ag,mk)
				       | Agent.Wildcard -> Rule.Init_mark (Agent.name ag,"undef")
				       | _ -> 
					   let s = "Not a valid initial internal state" in
					   runtime
					     (Some "simulation2.ml",
					      Some 461,
					      Some s)
					     s
				     and state_lnk = match lnk with
					 Agent.Bound -> (
					   try
					     let (j,s') = Solution.get_port (i,s) sol_init in
					       Rule.Init_bound (Agent.name ag,j,s'^"!")
					   with Not_found -> 
					     let s= "Simulation.init_net: not a valid initial link state" in
					     runtime 
					       (Some "simulation2.ml",
						Some 473,
						Some s)
					       s
					 )
				       | Agent.Free -> Rule.Init_free (Agent.name ag)
				       | _ -> 
					   let s = "Simulation.init_net: not a valid initial link state" in
					   runtime 
					     (Some "simulation2.ml",
					      Some 482,
					      Some s)
					     s
				     in
				       PortMap.add (i,s^"~") [state_inf] (PortMap.add (i,s^"!") [state_lnk] pmap)
				  ) ag modif
			     ) cc_i.Solution.agents PortMap.empty
	  in
	  let cc_str = Solution.kappa_of_solution cc_i
	  in
	  let net' = Network.add_intro ("intro:"^cc_str,modifs) true net
	  in
	    aux tl net' 
  in
    aux l_cc (Network.empty())
  
let concretize sim_data abs_pos_map abs_neg_map log =
  let pt = Unix.times() in
  let t = pt.Unix.tms_utime +. pt.Unix.tms_stime +. pt.Unix.tms_cutime +. pt.Unix.tms_cstime in
  let log = Session.add_log_entry 0 "--Concretization..." log in
  let pos_map,log = 
    Data_structures.IntMap.fold (fun i cplx_set (m,log) ->
				   let r_i,_ = 
				     try Rule_of_int.find i sim_data.rules 
				     with Not_found -> 
				       let s = "Simulation.concretize: incompatible rule indices" in
				       runtime
					 (Some "simulation2.ml",
					  Some 510,
					  Some s)
					 s
				   in
				   let splx_set,log =
				     Data_structures.IntSet.fold 
				       (fun j (splx_set,log) -> 
					  let r_j,_ = 
					    try Rule_of_int.find j sim_data.rules 
					    with Not_found -> 
					      let s="Simulation.concretize: incompatible rule indices" in
					      runtime
						(Some "simulation2.ml",
						 Some 523,
						 Some s)
						s
					  in
					    if (Rule.contains_deletion r_i) or (r_i << r_j)  
					    then (Mods2.IntSet.add j splx_set,log)
					    else 
					      let log = 
						let msg = "False positive(s) detected in activation map"
						in
						  Session.add_log_entry 4 msg log
					      in
						(splx_set,log)
				       ) cplx_set (Mods2.IntSet.empty,log)
				   in
				     (Mods2.IntMap.add i splx_set m,log)
				) abs_pos_map (Mods2.IntMap.empty,log)
  in
  let neg_map,log = 
    Data_structures.IntMap.fold (fun i cplx_set (m,log) ->
				   let r_i,_ = 
				     try Rule_of_int.find i sim_data.rules 
				     with Not_found -> 
				       let s="Simulation.concretize: incompatible rule indices" in
				       runtime
					 (Some "simulation2.ml",
					  Some 549,
					  Some s)
					 s
				   in
				   let splx_set,log =
				     Data_structures.IntSet.fold 
				       (fun j (splx_set,log) ->
					  if i=j then (splx_set,log) (*trivial inhibition relation*)
					  else
					    let r_j,_ = 
					      try Rule_of_int.find j sim_data.rules 
					      with Not_found -> 
						let s="Simulation.concretize: incompatible rule indices" in
						runtime
						  (Some "simulation2.ml",
						   Some 564,
						   Some s)
						  s
					    in
					      if (Rule.contains_deletion r_i) or (r_i %> r_j) 
					      then (Mods2.IntSet.add j splx_set,log)
					      else 
						let log = 
						  let msg = "False positive(s) detected in inhibition map"
						  in
						    Session.add_log_entry 4 msg log
						in
						  (splx_set,log)
				       ) cplx_set (Mods2.IntSet.empty,log)
				   in
				     (Mods2.IntMap.add i splx_set m,log)
				) abs_neg_map (Mods2.IntMap.empty,log)
  in
  let pt = Unix.times() in
  let t' = pt.Unix.tms_utime +. pt.Unix.tms_stime +. pt.Unix.tms_cutime +. pt.Unix.tms_cstime in
  let log = Session.add_log_entry 0 (Printf.sprintf "--Concretization: %f sec. CPU" (t'-.t)) log in  
    (pos_map,neg_map,log)

(*sim_data initialisation*)
let init log (rules,init,sol_init,obs_l,exp) =
  if !load_sim_data then
    try 
      let log = Session.add_log_entry 0 
	(Printf.sprintf "--Loading simulation state from %s..." !serialized_sim_data_file) log 
      in
      let d = open_in_bin (!serialized_sim_data_file) in 
      let time,f_sd = (Marshal.from_channel d: float * marshalized_sim_data_t) in
      let p = {max_failure = !Data.max_clashes;
	       init_sd = Some !serialized_sim_data_file ;
	       compress_mode = true ;
	       iso_mode = false ;
	       gc_alarm_high = false ;
	       gc_alarm_low = false 
	      }
      in
      let sd = unmarshal f_sd in
      let c = init_counters time sd in
      let log = Session.add_log_entry 0 "--Initial state successfully loaded." log in
	close_in d ;
	(log,p,{sd with lab = exp},{c with curr_time = time})
    with 
	exn -> 
	  let s = (Printf.sprintf "Could not load %s: %s" !serialized_sim_data_file (Printexc.to_string exn)) in
	    Error.runtime (None,None,None) s
  else
    let flag_obs,incomp = (*set of observable names and incompressible rule flags for storification*)
      List.fold_right (fun obs (obs_flg,incmp_flg) ->
			 match obs with
			     Solution.Occurrence flg -> if !story_mode then (obs_flg,incmp_flg) else (StringSet.add flg obs_flg,incmp_flg)
			   | Solution.Story (ncmp,flg) -> 
			       if !story_mode then 
				 let set = StringSet.add flg ncmp in
				   (StringSet.add flg obs_flg,StringSet.union set incmp_flg) 
			       else (obs_flg,incmp_flg)
			   | _ -> (obs_flg,incmp_flg)
		      ) obs_l (StringSet.empty,StringSet.empty)
    in
    let r_id = (List.length rules)+1 in
    let fake_rules,_ = 
      List.fold_right (fun obs (cont,fresh_id) ->
			 match obs with
			     Solution.Concentration (flg,sol) -> 
			       let actions = IntMap.empty
			       and lhs = Solution.split sol
			       in
			       let precompil = 
				 IntMap.fold (fun i cc_i map -> 
						IntMap.add i (Solution.recognitions_of_cc cc_i) map
					     ) lhs IntMap.empty 
			       in
			       let r =
				 {lhs = lhs ;
				  rhs = sol ; (*identity*)
				  precompil = precompil ;
				  add = IntMap.empty ;
				  actions = actions;
				  corr_ag = 0 ;
				  rate = -1 (*really geekish!*);
				  input = "obs" ; 
				  flag = Some flg ;
				  constraints = [] ;
				  kinetics = 1.0 ;
				  boost = 1.0 ;
				  automorphisms = None ;
				  n_cc = IntMap.size lhs ;
				  id = fresh_id;
				  infinite = false;
				  abstraction = None;
				  intra = None
				 }
			       in
				 (r::cont,fresh_id+1)
			   | Solution.Variable (flg,sol) -> 
			       let actions = IntMap.empty
			       and lhs = Solution.split sol
			       in
			       let precompil = 
				 IntMap.fold (fun i cc_i map -> 
						IntMap.add i (Solution.recognitions_of_cc cc_i) map
					     ) lhs IntMap.empty 
			       in
			       let r =
				 {lhs = lhs ;
				  rhs = sol ; (*identity*)
				  precompil = precompil ;
				  add = IntMap.empty ;
				  actions = actions;
				  corr_ag = 0 ;
				  rate = -1 (*really geekish!*);
				  input = "var" ; 
				  flag = Some flg ;
				  constraints = [] ;
				  kinetics = 1.0 ;
				  boost = 1.0 ;
				  automorphisms = None ;
				  n_cc = IntMap.size lhs ;
				  id = fresh_id;
				  infinite = false;
				  abstraction = None;
				  intra = None
				 }
			       in
				 (r::cont,fresh_id+1)
			   | _ -> (cont,fresh_id)
		      ) obs_l ([],r_id)
    in

    (************COMPLX INTERACTIONS**************)

    let pipeline_methods = Pipeline.methods () in 
    let _ = Config_complx.inhibition:=false in 
      (*converting simplx data structure to the complx one*)
    let cplx_log = pipeline_methods.Pipeline.empty_channel in 

    (*computing influence maps*)
    let (cplx_simplx:Pipeline.simplx_encoding) = 
      Some (fake_rules@rules,
	    init (*JK: list of pairs (sol,n) where n is the multiplication coef of sol*),
	    []   (*obs for ODE*)
              ,
            Experiment.unfun Experiment.empty
	   )  
    in
    let pb = pipeline_methods.Pipeline.build_pb cplx_simplx (add_suffix (add_suffix Tools.empty_prefix "") "")  
    in 
    let abs_pos_map,abs_neg_map,log,pb = 
      if (not !cplx_hsh) then (Data_structures.IntMap.empty,Data_structures.IntMap.empty,log,pb)
      else
	let pt = Unix.times() in
	let t = pt.Unix.tms_utime +. pt.Unix.tms_stime +. pt.Unix.tms_cutime +. pt.Unix.tms_cstime in
	let log = Session.add_log_entry 0 "--Computing abstraction of wake-up map..." log in
	let _ = Config_complx.dump_chrono:=false in 
	let _ = Config_complx.inhibition:=!Data.build_conflict in (*build negative map too*)
	  
	let pb,cplx_log = pipeline_methods.Pipeline.build_influence_map  "" "" "" 
	  (add_suffix (add_suffix (add_suffix Tools.empty_prefix "") "") "")  pb cplx_log 
	in 
	let log = Session.convert_cplx_log cplx_log log in
	let pt = Unix.times() in
	let t' = pt.Unix.tms_utime +. pt.Unix.tms_stime +. pt.Unix.tms_cutime +. pt.Unix.tms_cstime in
	let log = Session.add_log_entry 0 (Printf.sprintf "--Abstraction: %f sec. CPU" (t'-.t)) log in
	  match pb with 
	      None -> 
		let s="Simulation.init: complx did not return any maps, aborting" in
		  runtime
		    (Some "simulation2.ml",
		     Some 672,
		     Some s)
		    s
	    | Some pb' -> 
		let pos_map = 
		  match pb'.Pb_sig.wake_up_map with 
		      Some map -> map  
		    | None -> 
			let s="Simulation.init: no positive map, aborting" in
			  runtime
			    (Some "simulation2.ml",
			     Some 683,
			     Some s)
			    s
		and neg_map = 
		  match pb'.Pb_sig.inhibition_map with
		      Some map -> map  
		    | None -> Data_structures.IntMap.empty
		in
		  (pos_map,neg_map,log,pb) 
    in

    (*computing refinement quotient and automorphisms for real rules*)
    let (cplx_simplx:Pipeline.simplx_encoding) = 
      Some (rules,init,[],Experiment.unfun Experiment.empty)  
    in
    let pb = pipeline_methods.Pipeline.build_pb cplx_simplx (add_suffix (add_suffix Tools.empty_prefix "") "")  
    in 
    let rules,log,pb = 
      let enriched_rules,pb,cplx_log = 
	pipeline_methods.Pipeline.export_refinement_relation_maximal_and_automorphism_number 
	  (add_suffix (add_suffix (add_suffix Tools.empty_prefix "") "") "")
	  pb (pipeline_methods.Pipeline.empty_channel)
      in 
      let rules = (*replacing rules with enriched ones if computed*)
	match enriched_rules with
	    Some en_rules -> en_rules
	  | None -> let s="Simulation.init: failed to compute automorphisms for rules" in
	      Error.runtime
		(Some "simulation2.ml",
		 Some 718,
		 Some s)
		s
      in
      let log = Session.convert_cplx_log cplx_log log in
	(rules,log,pb)
    in

    (*computing automorphism for observables (fake rules)*)
    let (cplx_simplx:Pipeline.simplx_encoding) = 
      Some (fake_rules,init,[],Experiment.unfun Experiment.empty)
    in
    let pb = pipeline_methods.Pipeline.build_pb cplx_simplx (add_suffix (add_suffix Tools.empty_prefix "") "")  
    in 
    let fake_rules,log,pb = 
      let enriched_rules,pb,cplx_log = 
	pipeline_methods.Pipeline.export_automorphism_number 	
	  (add_suffix (add_suffix (add_suffix Tools.empty_prefix "") "") "")
	  pb (pipeline_methods.Pipeline.empty_channel)
      in 
      let fake_rules = (*replacing rules with enriched ones if computed*)
	match enriched_rules with
	    Some en_rules -> en_rules
	  | None -> 
	      let s="Simulation.init: failed to compute automorphisms for observables" in
		Error.runtime
		  (Some "simulation2.ml",
		   Some 751,
		   Some s)
		  s
      in
      let log = Session.convert_cplx_log cplx_log log in
	(fake_rules,log,pb)
    in

    (*****End COMPLX interactions***********)

    let _ = Gc.full_major() in
    let rule_list = fake_rules@rules in 
    let nrule = List.length rule_list in
    let sim_data = {(sd_empty ()) with rules = Rule_of_int.empty nrule} in
    let sim_data = List.fold_left (fun sd r -> 
				     match r.flag with
					 None -> add_rule false r sd 
				       | Some flg -> (*might be a fake rule*)
					   if StringSet.mem flg flag_obs then (*to be observed*)
					     add_rule true r sd 
					   else (*rule with a name but not to be observed*)
					     add_rule false r sd 
				  ) {sim_data with sol = sol_init} rule_list
    in
      (*Positive and negative map construction*)
    let log,sim_data = 
      if not !cplx_hsh then (log,sim_data) (*done in add_rule*)
      else
	let pos_map,neg_map,log = concretize sim_data abs_pos_map abs_neg_map log in
	let log = Session.add_log_entry 0 (Printf.sprintf "--Influence map computed") log 
	in
	  (log,{sim_data with flow = pos_map ; conflict = neg_map})
    in
    let p = {max_failure = !Data.max_clashes;
	     init_sd = None;
	     compress_mode = true ;
	     iso_mode = false ;
	     gc_alarm_high = false ;
	     gc_alarm_low = false 
	    }
    in
    let incomp_ids = StringSet.fold (fun flg set -> IntSet.add (StringMap.find flg sim_data.rule_of_name) set) incomp IntSet.empty in
      (log,p,{sim_data with 
		n_ag = sol_init.Solution.fresh_id ; 
		lab = exp ;
		incompressible = IntSet.union sim_data.incompressible incomp_ids},
       init_counters 0.0 sim_data)

let mult_kinetics flg mult sim_data =
  try
    let i = StringMap.find flg sim_data.rule_of_name in
    let r_i,inst_i = Rule_of_int.find i sim_data.rules in
    let r_i'={r_i with kinetics = r_i.kinetics *. mult} in
      {sim_data with rules = Rule_of_int.add i (r_i',inst_i) sim_data.rules}
  with Not_found -> 
    let s=("Simulation.mult_kinetics: label "^flg^" does not match any rule.") in
    Error.runtime 
      (Some "simulation2.ml",
       Some 815,
       Some s)
      s

exception Intra of assoc
exception Unary

let rec bologna (abst_r,abst_ind) (conc_r,conc_ind) (i_phi,nb_phi,phi) (i_psi,nb_psi,psi) sim_data log = 
  let try_intra = test_intra conc_r in
  let _ = 
    if !debug_mode then (Printf.printf "[*] Checking validity of injections [%d,%d,%d],[%d,%d,%d]\n" 
			   abst_ind i_psi nb_psi abst_ind i_phi nb_phi) else ()
  in
    (*0. COMPUTING P_INTRA*)
  let n_phi = 
    let coord = Coord.of_pair (abst_ind,i_phi) in
    let _,_,m = InjArray.find coord sim_data.injections in
      float_of_int (AssocArray.size m)
  in
  let n_psi = 
    let coord = Coord.of_pair (abst_ind,i_psi) in
    let _,_,m = InjArray.find coord sim_data.injections in
      float_of_int (AssocArray.size m)
  in
  let n_inj = (n_psi/. !rescale) +. (n_phi/. !rescale) in
  let p_intra = (*p_intra is 0.0 if rule is infinite or no intra rate is defined*)
    if (conc_r.kinetics < 0.0) then 0.0
    else
      match conc_r.intra with
	  Some u -> begin u /. (conc_r.boost *. n_inj) end
	| None -> 0.0
  in (*u=coef_unary*beta and p = u/beta.(|phi|+|psi|-1)*)
  let log,p_intra,boost = 
    if try_intra then
      let u = match conc_r.intra with Some u -> u | None -> 
	let s= "Simulation.bologna: intra rate not specified" in
	Error.runtime 
	  (Some "simulation2.ml",
	   Some 853,
	   Some s)
	  s
      in
      let boost = u /. n_inj in
	if (p_intra > 1.0) then
	  let log = Session.add_log_entry 4 (Printf.sprintf "Rule %s: boosting binary rate to catch up" (Rule.name conc_r)) log 
	  in
	    (log,1.0,boost)
	else 
	  if (boost >= conc_r.kinetics) && (boost < conc_r.boost) then (*decreasing boost if possible*)
	    let log = Session.add_log_entry 4 (Printf.sprintf "Rule %s: decreasibg boosted binary rate" (Rule.name conc_r)) log 
	    in
	      (log,p_intra,boost)
	  else
	    (log,p_intra,conc_r.boost)
    else
      (log,p_intra,conc_r.boost)
  in


  (*1. CHECKING FOR PURE COLLISION*)
  let phi_psi_opt = try Some (merge_injections phi psi) with Not_found -> None 
  in
    if not try_intra && (match conc_r.Rule.constraints with ([Rule.ROOTED_STORY _]|[]) -> true | _ -> false)
    then  (*if not looking for depolymerization, then no clash is enough to answer*)
      match phi_psi_opt with
	  Some inj -> 
	    if boost > conc_r.kinetics then 
	      let _ =
		if !debug_mode then (Printf.printf "Binary rate overestimated, correcting with p_inter = %f\n" (conc_r.kinetics /.boost) ; flush stdout) else ()
	      in
	      let dice = Random.float 1.0 in
		if dice < (conc_r.kinetics /. boost) then ([inj],log,None,boost) (*add here p_inter*)
		else
		  let _ =
		    if !debug_mode then (Printf.printf "Rejected\n" ; flush stdout) else ()
		  in
		    ([],log,None,boost)
	    else
	      ([inj],log,None,boost)
	| None -> 
	    let _ = if !debug_mode then (Printf.printf "Clash by non injectivity of maps\n" ; flush stdout) else ()
	    in
	      ([],log,None,boost) (*clash by collision*)    
    else
      if phi_psi_opt = None then 
	let _ = if !debug_mode then (Printf.printf "Clash by non injectivity of maps\n" ; flush stdout) else ()
	in
	  ([],log,None,boost) 
      else
	
	(*2. Unary vs. binary*)
	let _,phi_0 = IntMap.get phi 
	and _,psi_0 = IntMap.get psi
	in
	let cc_phi,paths_phi = Solution.paths_of_id phi_0 sim_data.sol (*extending codomain of phi to the whole CC*)
	and cc_psi,paths_psi = Solution.paths_of_id psi_0 sim_data.sol (*extending codomain of phi to the whole CC*)
	in
	let paths_phi,paths_psi = (*adding the newly created bond to the paths*)
	  IntMap.fold (fun id act_msg_list (paths_phi,paths_psi) ->
			 let l = Solution.get_binding act_msg_list in
			 let id',i = 
			   try (IntMap.find id phi,0)
			   with Not_found -> 
			     try (IntMap.find id psi,1) with Not_found -> 
			       let s = "Simulation.bologna: Agent is not in the image of any injections!" in
			       Error.runtime 
				 (Some "simulation2.ml",
				  Some 922,
				  Some s)
				 s
			 in
			 let name = Agent.name (Solution.agent_of_id id' sim_data.sol) in
			   List.fold_left (fun (paths_phi,paths_psi) (site,id2,site2) -> 
					     if i=0 (*action on the phi side*) then
					       let id2 = try IntMap.find id2 psi with Not_found -> let s="Simulation.bologna: malformed injection" in
					       Error.runtime
						 (Some "simulation2.ml",
						  Some 932,
						  Some s)
						 s
					       in
					       let name' = Agent.name (Solution.agent_of_id id2 sim_data.sol) in
					       let paths_phi = Paths.add name id site paths_phi
					       and paths_psi = Paths.add name' id2 site2 paths_psi
					       in
						 (paths_phi,paths_psi)  
					     else (*action on the psi side*)
					       let id2 = try IntMap.find id2 phi with Not_found -> 
						 let s = "Simulation.bologna: malformed injection" in
						 Error.runtime
						   (Some "simulation2.ml",
						    Some 946,
						    Some s)
						   s
					       in
					       let name' = Agent.name (Solution.agent_of_id id2 sim_data.sol) in
					       let paths_psi = Paths.add name id site paths_psi
					       and paths_phi = Paths.add name' id2 site2 paths_phi
					       in
						 (paths_phi,paths_psi)
					  ) (paths_phi,paths_psi) l
		      ) abst_r.Rule.actions (paths_phi,paths_psi)
	in
	let _ =
	  if !debug_mode then Printf.printf "-Injective map, testing now whether constraints are satisfied...\n" else ()
	in
	let co_psi = IntMap.fold (fun _ j codomain -> IntSet.add j codomain) psi IntSet.empty
	and co_phi = IntMap.fold (fun _ j codomain -> IntSet.add j codomain) phi IntSet.empty
	in
	let no_helix,no_poly = List.fold_left (fun (no_helix,no_poly) cstr -> 
						 match cstr with
						     NO_HELIX -> (true,no_poly)
						   | NO_POLY -> (no_helix,true)
						   | _ -> (no_helix,no_poly)
					      ) (false,false) conc_r.constraints
	in
	let clash = Paths.clashing_on_names ~debug:(!debug_mode) (no_helix,no_poly) paths_psi paths_phi
	in
	  
	  (*rule is not clashing according to constraints *)
	  if not clash then 
	    if IntSet.is_empty (IntSet.inter cc_phi cc_psi) then (*not clashing and binary!*)
	      match phi_psi_opt with
		  Some inj -> 
		    let _ =
		      if !debug_mode then Printf.printf "-Reaction satisfies constraints...\n" else ()
		    in
		      if boost > conc_r.kinetics then 
			let _ =
			  if !debug_mode then (Printf.printf "But binary rate overestimated, correcting with p_inter = %f\n" (conc_r.kinetics /. boost) ; flush stdout) else ()
			in
			let dice = Random.float 1.0 in
			  if dice < (conc_r.kinetics /. boost) then ([inj],log,None,boost) (*add here p_inter*)
			  else
			    let _ =
			      if !debug_mode then (Printf.printf "Rejected\n" ; flush stdout) else ()
			    in
			      ([],log,None,boost)
		      else
			([inj],log,None,boost)
		| None -> let s="Simulation.bologna: injections are not clashing but cannot be composed!" in
		  Error.runtime
		    (Some "simulation2.ml",
		     Some 997,
		     Some s)
		    s
	    else (*not clashing and unary!*)
	      begin
		let _ =
		  if !debug_mode then (Printf.printf "Unary reaction...\n" ; flush stdout) else () 
		in
		let dice = Random.float 1.0 in
		  if dice < p_intra then
		    match phi_psi_opt with
			Some inj -> ([inj],log,Some p_intra,boost)
		      | None -> 
			  let s="Simulation.bologna: injections are not clashing but cannot be composed!"  in
			  Error.runtime 
			    (Some "simulation2.ml",
			     Some 1013,
			     Some s)
			    s
		  else ([],log,Some p_intra,boost)
	      end
	  else

	    (*3. trying to resolve clashes using intras*)

	    let intra ids indice_cc inj_number map co_map = 
	      try
		let _ =
		  IntSet.fold (fun id inj_blacklist ->
				 let ag = Solution.agent_of_id id sim_data.sol in
				   if IntSet.mem id co_map then inj_blacklist 
				   else
				     Agent.fold_interface (fun site _  inj_blacklist -> 
							     let coordSet = try CoordSetArray.find (id,site^"!") sim_data.lift with Not_found -> CoordSet.empty 
							     in
							       CoordSet.fold (fun (coord,inj_number') inj_blacklist -> 
										let (indice_r',indice_cc') = Coord.to_pair coord in
										  if (indice_r' = abst_ind) && (indice_cc'=indice_cc) then 
										    let _,_,assoc_map = InjArray.find coord sim_data.injections in
										      if inj_number' = inj_number then
											let _ =
											  if !debug_mode then (Printf.printf " but context says the reaction is unary!\n" ; flush stdout) 
											  else ()
											in
											  raise Unary
										      else
											let inj = AssocArray.find inj_number' assoc_map in
											let merge_inj = try Some (merge_injections inj map) with Not_found -> None
											in
											let inj_blacklist = IntSet.add inj_number' inj_blacklist in
											  match merge_inj with
											      Some ga -> 
												let _ = 
												  if !debug_mode then 
												    (Printf.printf " with injection [%d,%d,%d]\n" abst_ind indice_cc inj_number' ; flush stdout)
												  else () 
												in
												  raise (Intra ga)
											    | None -> inj_blacklist
										  else inj_blacklist
									     ) coordSet inj_blacklist
							  ) ag inj_blacklist
			      ) ids IntSet.empty
		in
		let _ = if !debug_mode then 
		  (Printf.printf " but cannot find any candidate\n" ; flush stdout ) else () 
		in
		  (None,false)
	      with 
		  Intra inj -> (Some inj,false)
		| Unary -> (None,true) 
	    in
	    let _ = if !debug_mode then (Printf.printf "Reaction does not satisfy constraints, trying to exchange [%d,%d,%d]..." abst_ind i_phi nb_phi ; flush stdout) else () 
	    in
	    let opt_intra_phi,unary = intra cc_psi i_phi nb_phi psi co_psi in
	      if unary then
		let dice = Random.float 1.0 in
		let log = Session.add_log_entry 4 (Printf.sprintf "Unary case of binary rule r[%d] no need to apply intra\n" abst_ind) log in
		  if dice<p_intra then 
		    match phi_psi_opt with
			Some inj -> ([inj],log,Some p_intra,boost)
		      | None -> let s=  "Simulation.bologna: rule should not be clashing" in
			Error.runtime
			  (Some "simulation2.ml",
			   Some 1081,
			   Some s)
			  s
		  else 
		    let _ = if !debug_mode then (Printf.printf "Reject (p_intra = %f)\n" p_intra ; flush stdout) else () 
		    in
		      ([],log,Some p_intra,boost) 
	      else
		let _ = if !debug_mode then Printf.printf "Trying to exchange [%d,%d,%d]..." abst_ind i_psi nb_psi ; flush stdout in
		let opt_intra_psi,_ = intra cc_phi i_psi nb_psi phi co_phi in
		  match (opt_intra_phi,opt_intra_psi) with
		      (None,None) -> ([],log,None,boost) (*clash because no intra could be found*)
		    | (Some ga,None) -> 
			let dice = Random.float 1.0 in
			  if dice < p_intra then ([ga],log,Some p_intra,boost) 
			  else 
			    let _ = if !debug_mode then (Printf.printf "Reject (p_intra = %f)\n" p_intra ; flush stdout) else () 
			    in 
			      ([],log,Some p_intra,boost)
		    | (Some ga0,Some ga1) ->
			let dice1 = Random.float 1.0 in
			let dice2 = Random.float 1.0 in
			let l = if dice1 < p_intra then [ga0] else 
			  let _ = if !debug_mode then (Printf.printf "Reject first intra (p_intra = %f)\n" p_intra ; flush stdout) else () 
			  in
			    [] 
			in
			let l = 
			  if dice2<p_intra then ga1::l else 
			    let _ = if !debug_mode then (Printf.printf "Reject second intra (p_intra = %f)\n" p_intra ; flush stdout) else () 
			    in
			      l 
			in
			let log = Session.add_log_entry 4 "Multiple intras detected" log in (l,log,Some p_intra,boost)
		    | (None,Some ga) -> 
			let dice = Random.float 1.0 in
			  if dice<p_intra then ([ga],log,Some p_intra,boost)
			  else
			    let _ = if !debug_mode then (Printf.printf "Reject (p_intra = %f)\n" p_intra ; flush stdout) else () 
			    in
			      ([],log,Some p_intra,boost)

exception Not_applicable of int*Rule.t
exception Found of int*Rule.t
exception Assoc of int IntMap.t

let select log sim_data p c =
  let get_map_opt map = 
    let (k,assoc) = AssocArray.random map in (k,assoc)
  in
  let activity = Rule_of_int.accval sim_data.rules 
  in
  let _ = if !debug_mode then Printf.printf "Activity:%f\n" activity ; flush stdout in
  let rec choose_rule log sim_data max_failure cpt = (*cpt = nb clashes*)
    if activity <= !Data.deadlock_sensitivity then 
      let log = Session.add_log_entry 1 ("Activity has reached zero") log 
      in
	(log,None,sim_data,cpt)
    else
      let (abst_ind,abst_r),(conc_ind,conc_r),inst,log = 
	try (*first trying to pick an infinitely fast rule*)
	  let conc_ind = IntSet.choose sim_data.inf_list in
	  let (conc_r,inst) = Rule_of_int.find conc_ind sim_data.rules
	  in
	  let abst_r,log = 
	    if !Data.quotient_refs then 
	      match conc_r.Rule.abstraction with
		  None -> 
		    let log = Session.add_log_entry 1 (Printf.sprintf "Abstraction not computed for rule %s" (Rule.name conc_r)) log 
		    in
		      (conc_r,log)
		| Some l_abst -> 
		    begin
		      match l_abst with
			  [] -> (conc_r,log)
			| [abst_r] -> 
			    let log = Session.add_log_entry 1 (Printf.sprintf "Using abstraction for rule %s" (Rule.name conc_r)) log 
			    in
			      (abst_r,log)
			| _ -> (*several choices of abstraction, so making no choice at all*)
			    let log = Session.add_log_entry 1 (Printf.sprintf "Not a unique abstraction for rule %s" (Rule.name conc_r)) log 
			    in
			      (conc_r,log)
		    end
	    else (conc_r,log) (*if not quotienting refinements then abst rule is also the concrete one*)
	  in
	  let abst_ind = 
	    if !Data.quotient_refs then StringMap.find (Rule.name abst_r) sim_data.rule_of_name 
	    else conc_ind
	  in
	    ((abst_ind,abst_r),(conc_ind,conc_r),inst,log)
	with Not_found -> (*no infinite rule applies*)
	  begin
	    let conc_ind,(conc_r,inst) = 
	      try Rule_of_int.random_val sim_data.rules 
	      with exn -> let s = ("Simulation.select: from Rule_of_int.random_val, received "^(Printexc.to_string exn)) in
		Error.runtime
		  (Some "simulation2.ml",
		   Some 1176,
		   Some s)
		  s
	    in
	    let abst_r,log =
	      if !Data.quotient_refs then 
		(*replacing rule r with its abstraction*)
		begin
		  match conc_r.Rule.abstraction with
		      None -> 
			let log = Session.add_log_entry 1 
			  (Printf.sprintf "Abstraction not computed for rule %s" (Rule.name conc_r)) log 
			in
			  (conc_r,log)
		    | Some l_abst -> 
			begin
			  match l_abst with
			      [] -> (conc_r,log)
			    | [abst_r] -> (abst_r,log)
			    | _ -> (*several choices of abstraction, so making no choice at all*)
				let log = Session.add_log_entry 1 (Printf.sprintf "Not a unique abstraction for rule %s" (Rule.name conc_r)) log 
				in
				  (conc_r,log)
			end
		end
	      else (conc_r,log)
	    in
	      try
		let abst_ind = 
		  if !Data.quotient_refs then StringMap.find (Rule.name abst_r) sim_data.rule_of_name 
		  else conc_ind
		in
		  ((abst_ind,abst_r),(conc_ind,conc_r),inst,log)
	      with
		  Not_found -> 
		    let s=("Simulation.select: Rule "^(Rule.name abst_r)^" not found") in 
		      Error.runtime 
			(Some "simulation2.ml",
			 Some 1204,
			 Some s)
			s
	  end
      in
	if conc_r.infinite then (*selection of an instance of an infinitely fast rule*)
	  let assoc_map_list = 
	    IntMap.fold (fun i lhs_i cont ->
			   let _,_,assoc_map_i = InjArray.find (Coord.of_pair (abst_ind,i)) sim_data.injections in
			     if AssocArray.size assoc_map_i = 0 then 
			       let s="Simulation.select: rule has an infinite activity but the image of a cc is empty" in
				 Error.runtime
				   (Some "simulation2.ml",
				    Some 1229,
				    Some s)
				   s
			     else
			       assoc_map_i::cont
			) abst_r.lhs []
	  in
	  let product l = 
	    List.fold_left (fun prod assoc_ar ->
			      List.fold_left (fun prod (prod_map_i,inv_prod_map_i) -> 
						let ext = 
						  AssocArray.fold (fun _ map_j prod ->
								     try 
								       let (m,im) = 
									 IntMap.fold (fun i j (map,invmap) -> 
											if IntMap.mem j invmap then raise (Not_applicable (1,abst_r)) 
											  (*collision*)
											else
											  (IntMap.add i j map,IntMap.add j i invmap)
										     ) map_j (prod_map_i,inv_prod_map_i)
								       in
									 (m,im)::prod
								     with Not_applicable _ -> prod
								  ) assoc_ar []
						in
						  ext@prod
					     ) [] prod
			   ) [(IntMap.empty,IntMap.empty)] l
	  in
	  let prod = product assoc_map_list in
	    match prod with
		[] -> 
		  let log = Session.add_log_entry 1 (Printf.sprintf "Infinite rule r[%d] is discarded because it has only clashing instances" conc_ind) log 
		  in
		    choose_rule log {sim_data with 
				       inf_list = IntSet.remove conc_ind sim_data.inf_list ;
				       rules = Rule_of_int.add conc_ind (conc_r,0.0) sim_data.rules 
				    } max_failure cpt
	      | (m,_)::_ -> 
		  let log = Session.add_log_entry 4 (Printf.sprintf "Applying infinitely fast rule r[%d]" conc_ind) log 
		  in
		    (log,Some (abst_ind,conc_ind,[m]),sim_data,cpt) (*cpt doesn't matter since infinite rate rule and time advance is null*)
	else
	  try
	    let inj_list,arity =
	      IntMap.fold (fun i lhs_i (inj_list,length) -> 
			     try
			       let free_keys,fresh,assoc_map_i = 
				 InjArray.find (Coord.of_pair (abst_ind,i)) sim_data.injections in
			       let length_i = AssocArray.size assoc_map_i in
				 if length_i = 0 then raise (Not_applicable (-1,abst_r)) 
				 else
				   let inj_nb,phi_i = get_map_opt assoc_map_i 
				   in
				     ((inj_nb,i,phi_i)::inj_list,length+1)
			     with
				 Not_found -> raise (Not_applicable (0,abst_r))  (*not applicable because one cc has no injection*)
				   
			  ) (abst_r.lhs) ([],0) (*IntMap.empty,IntMap.empty*)
	    in
	    let inj_list,log,opt_intra,boost = 
	      (* JK inj_list is empty if rule has no left hand side*)
	      if inj_list = [] then 
		let s= "Simulation2.select: this version of the simulator does not accept rules with no left hand side!" in
		  Error.runtime 
		    (Some "simulation2.ml",
		     Some 1294,
		     Some s)
		    s
	      else
		if arity = 1 then 
		  match inj_list with 
		      [(_,_,phi)] -> ([phi],log,None,conc_r.boost) 
		    | _ -> let s= "Simulation2.select: invalid argument" in
			Error.runtime 
			  (Some "simulation2.ml",
			   Some 1305,
			   Some s)
			  s
		else
		  match inj_list with
		      [(psi_nb,cc_psi,psi);(phi_nb,cc_phi,phi)] -> 
			bologna (abst_r,abst_ind) (conc_r,conc_ind) (cc_psi,psi_nb,psi) (cc_phi,phi_nb,phi) sim_data log
		    | l -> 
			let phi = 
			  List.fold_left (fun phi (_,_,psi) -> 
					    let opt = 
					      try Some (merge_injections phi psi) with Not_found -> None
					    in
					      match opt with
						  None -> raise (Not_applicable (2,abst_r)) 
						| Some ga -> ga
					 ) IntMap.empty l
			in
			  ([phi],log,None,conc_r.boost) 
	    in
	      if !plot_p_intra then 
		begin
		  match opt_intra with
		      Some p -> 
			let d = match !prob_desc with
			    None -> 
			      let d = open_out !p_intra_fic in
				prob_desc:=Some d ;
				d
			  | Some d -> d
			in
			  Printf.fprintf d "%f %f\n" c.curr_time p ;
		    | None -> ()
		end ;
	      match inj_list with
		  [] -> raise (Not_applicable (2,abst_r))
		| [ga] -> 
		    let rules = Rule_of_int.add conc_ind ({conc_r with boost = boost},inst) sim_data.rules in
		    let sd = {sim_data with rules = rules} in
		      (log,Some (abst_ind,conc_ind,[ga]),sd,cpt)			
		| [ga0;ga1] -> 
		    let compatible = try let _ = merge_injections ga0 ga1 in true with Not_found -> false
		    in
		      if compatible then 
			let rules = Rule_of_int.add conc_ind ({conc_r with boost = boost},inst) sim_data.rules in
			let sd = {sim_data with rules = rules} in
			  (log,Some (abst_ind,conc_ind,[ga0;ga1]),sd,cpt)
		      else 
			begin
			  let log = Session.add_log_entry 4 "Selected pair of intras are conflicting, discarding one." log in
			  let rules = Rule_of_int.add conc_ind ({conc_r with boost = boost},inst) sim_data.rules in
			  let sd = {sim_data with rules = rules} in
			    (log,Some (abst_ind,conc_ind,[ga0]),sd,cpt)
			end
		| _ -> let s="Simulation.selec: invalid argument" in
		    Error.runtime
		      (Some "simulation2.ml",
		       Some 1355,
		       Some s)
		      s
	  with 
	      Not_applicable (-1,r) -> let s="Simulation.select: selected rule has no injection (error -1)" in
		runtime
		  (Some "simulation2.ml",
		   Some 1365,
		   Some s) 
		  s
	    | Not_applicable (0,r) -> let s=(Printf.sprintf "Simulation.select: selected rule %s has no injection (error 0)" (r.Rule.input)) in
		runtime
		  (Some "simulation2.ml",
		   Some 1371,
		   Some s)
		  s
	    | Not_applicable (1,r) -> 
		let log = Session.add_log_entry 4 (Printf.sprintf "Clash in rule %s" (Rule.name r)) log in
		  if max_failure < 0 then choose_rule log sim_data max_failure (cpt+1)
		  else 
		    if cpt < max_failure then choose_rule log sim_data max_failure (cpt+1) 
		    else 
		      let log = Session.add_log_entry 1 ("Max failure reached: "^(Rule.name r)) log 
		      in
			(log,None,sim_data,max_failure)
	    | Not_applicable (2,r) ->
		let log = Session.add_log_entry 4 
		  (Printf.sprintf "Application of rule %s is clashing" (Rule.name r)) log in
		  if max_failure < 0 then choose_rule log sim_data max_failure (cpt+1)
		  else 
		    if cpt < max_failure then choose_rule log sim_data max_failure (cpt+1) 
		    else 
		      let log = Session.add_log_entry 1 ("Max failure reached: "^(Rule.name r)) log 
		      in
			(log,None,sim_data,max_failure)
			  
  in
    if (Rule_of_int.accval sim_data.rules) < sim_data.min_rate then (log,None,sim_data,0)
    else
      choose_rule log sim_data p.max_failure 0

(*Test whether assoc already belongs to assoc_map*)
let consistency_check assoc assoc_map = 
  try 
    (*IntMap.fold (fun _ m consistent -> *)
    AssocArray.fold (fun _ m consistent -> 
		       let identique = try 
			 IntMap.fold (fun i j identique -> 
					let j' = IntMap.find i assoc in if j=j' then identique else
					    raise False ) m true 
		       with False | Not_found -> false 
		       in
			 if identique then raise False 
			 else consistent 
		    ) assoc_map true
  with False -> false
    

    
let update warn r_ind assoc upd_quarks assoc_add sol sim_data p c = (*!! r_ind is the indice of the abstract rule when quotient_refs is enabled !!*)
  if !debug_mode then Printf.printf "modified quarks: %s\n" (Mods2.string_of_set string_of_port PortSet.fold upd_quarks) ;

  (*negative update*)
  let t_neg = chrono 0.0 in
  let lift,injs,rules,rm_coord,mod_ids,mod_obs,inf_list = 
    PortSet.fold (fun (i,s) (lift,injs,rules,rm_coord,mod_ids,mod_obs,inf_list) ->
		    let mod_ids = (*contains modified agent id (but not deleted agent ids)*)
		      if Solution.AA.mem i sol.Solution.agents 
		      then Solution.AA.add i (Solution.agent_of_id i sol) mod_ids
		      else mod_ids
		    in
		    let coord_injs = (*injection coordinates using quark (i,s)*)
		      try 
			CoordSetArray.find (i,s) lift 
		      with Not_found -> CoordSet.empty (*lift already removed*)
		    in
		    let injs,rules,rm_coord,mod_obs,inf_list = 
		      CoordSet.fold 
			(fun (coord,assoc_ind) (injs,rules,rm_coord,mod_obs,inf_list) -> 
			   try
			     let rule_ind,cc_ind = Coord.to_pair coord in
			     let free_keys,fresh,map = 
			       InjArray.find (Coord.of_pair (rule_ind,cc_ind)) injs 
			     in
			     let size =  AssocArray.size map in
			       if size = 0 then 
				 begin 
				   let s="Simulation.update: map size error" in
				     Error.runtime 
				       (Some "simulation2.ml",
					Some 1450,
					Some s)
				       s
				       
				 end;
			       let length = float_of_int size
			       and length'= float_of_int (size - 1) 
			       in
			       let assoc = AssocArray.find assoc_ind map in
			       let map = AssocArray.remove assoc_ind map in
			       let t0 = chrono 0.0 in
			       let r,inst_rule_ind = Rule_of_int.find rule_ind rules in
			       let _ = if !Mods2.bench_mode then Bench.t_upd_rules:=!Bench.t_upd_rules +. (chrono t0) in
			       let inst_rule_ind' = (inst_rule_ind /. length) *. length' in
			       let t0 =  chrono 0.0 in
				 (*modifying rule kinetics if necessary*)
			       let boost = 
				 let phi = length'
				 and psi = 
				   try
				     let _,_,map'= InjArray.find (Coord.of_pair (rule_ind,(cc_ind + 1) mod 2)) injs in
				       float_of_int (AssocArray.size map')
				   with Not_found -> 0.0 (*cc has no injection or unary rule*)
				 in
				   match r.intra with 
				       None -> r.kinetics
				     | Some u -> 
					 if phi = 0. or psi = 0. then r.kinetics (*no need to boost, rule is dead*)
					 else
					   max (u /. (phi +. psi)) (r.kinetics) 
			       in
			       let rules = Rule_of_int.add rule_ind ({r with boost=boost},inst_rule_ind') rules in
			       let inf_list = (*removing infinite rate rule with no more instances*)
				 if ((int_of_float inst_rule_ind') = 0) && (IntSet.mem rule_ind inf_list) then IntSet.remove rule_ind inf_list
				 else inf_list
			       in
			       let _ = if !Mods2.bench_mode then Bench.t_upd_rules:= !Bench.t_upd_rules +. (chrono t0) in
			       let mod_obs = 
				 (*CORRECTION BUG 11 dec 2007*)
				 if IntSet.mem rule_ind sim_data.obs_ind then 
				   IntSet.add rule_ind mod_obs
				 else mod_obs
				   (*FIN CORRECTION BUG 11 dec 2007*)
			       in
				 if AssocArray.is_empty map then  
				   (InjArray.remove coord injs, 
				    rules, 
				    ((coord,assoc_ind),assoc)::rm_coord,
				    mod_obs,
				    inf_list
				   )
				 else 
				   (InjArray.add coord (assoc_ind::free_keys,fresh,map) injs, 
				    rules, 
				    ((coord,assoc_ind),assoc)::rm_coord,
				    mod_obs,
				    inf_list
				   )
			   with Not_found -> (injs,rules,rm_coord,mod_obs,inf_list) (*injection already removed*)
			) coord_injs (injs,rules,rm_coord,mod_obs,inf_list) 
		    in
		    let lift = 
		      CoordSetArray.remove (i,s) lift 
		    in 
		      (lift,injs,rules,rm_coord,mod_ids,mod_obs,inf_list)
		 ) upd_quarks (sim_data.lift,sim_data.injections,sim_data.rules,
			       [], (*rm_coord*)
			       (Implementation_choices.Clean_solution.alloc_solution solution_AA_create), (*mod_ids*)
			       IntSet.empty (*mod_obs*),
			       sim_data.inf_list
			      )
  in
  let mod_ids = IntMap.fold (fun _ i m -> Solution.AA.add i (Solution.agent_of_id i sol) m) assoc_add mod_ids in
  let lift = 
    List.fold_right
      (fun ((coord,assoc_k),assoc) lift -> (*fold on rm_coord*)
	 try
	   IntMap.fold (fun id_p id_sol lift -> (*fold on assoc --whose id is assoc_k but has been removed*)
			  try
			    let ag = 
			      Solution.AA.find id_sol sol.Solution.agents 
			    in
			      Agent.fold_interface  (fun s _ lift ->
						       let lift =
							 let cset = 
							   try 
							     CoordSetArray.find (id_sol,s^"!") lift 
							   with Not_found -> CoordSet.empty
							 in
							 let cset = 
							   CoordSet.remove (coord,assoc_k) cset
							 in
							   if CoordSet.is_empty cset then 
							     CoordSetArray.remove (id_sol,s^"!") lift 
							   else
							     CoordSetArray.add (id_sol,s^"!") cset lift
						       in
						       let cset = 
							 try 
							   CoordSetArray.find (id_sol,s^"~") lift 
							 with Not_found -> CoordSet.empty
						       in
						       let cset = 
							 CoordSet.remove (coord,assoc_k) cset
						       in
							 if CoordSet.is_empty cset then 
							   CoordSetArray.remove (id_sol,s^"~") lift
							 else
							   CoordSetArray.add (id_sol,s^"~") cset lift
						    ) ag lift
			  with Not_found -> lift (*agent has been removed so all its quarks were modified*)
		       ) assoc lift
	 with Not_found -> let s="Simulation.update: rm_coord invariant violation" in
	   Error.runtime 
	     (Some "simulation2.ml",
	      Some 1572,
	      Some s)
	     s
      ) rm_coord lift
  in
  let _ = if !bench_mode then Bench.neg_upd := !Bench.neg_upd +. (chrono t_neg) in
    
  (*positive update*)    
    
  let t_pos = chrono 0.0 in
    (*candidates rules to have new injs using mod_quarks are r' such that r<<r' *)
  let next = 
    if warn && (not !cplx_hsh) then (*if rule contains a deletion and abstract positive map was not computed*)
      Rule_of_int.fold (fun i _ set -> IntSet.add i set) sim_data.rules IntSet.empty
    else
      try IntMap.find r_ind sim_data.flow with Not_found -> IntSet.empty in
    
  let assoc_list = 
    IntSet.fold (*fold on all rules that might be woken up*)
      (fun rule_ind assoc_list ->
	 let t0 = chrono 0.0 in
	 let r_i,_ = 
	   try Rule_of_int.find rule_ind sim_data.rules 
	   with Not_found -> let s="Simulation.update: invalid rule indice" in
	     runtime
	       (Some "simulation2.ml",
		Some 1598,
		Some s)
	       s
	 in
	 let _ = if !bench_mode then Bench.t_upd_rules:=!Bench.t_upd_rules +. (chrono t0) in
	   if !debug_mode then Printf.printf "waking up r[%d]\n" rule_ind ; 
	   IntMap.fold (fun ind_cc lhs_i assoc_list -> (*for all cc[i] of the rule*)
			  try
			    let precompil = IntMap.find ind_cc r_i.precompil in
			    let assoc_map_lhs_sol = Solution.unify (precompil,lhs_i) (mod_ids,sol) in
			      if !debug_mode then Printf.printf "new injection(s) found\n" ;
			      AssocArray.fold
				(fun _ assoc_lhs_sol assoc_list ->
				   let quarks = 
				     IntMap.fold 
				       (fun i_lhs j_sol set ->
					  try
					    let ag_i = Solution.agent_of_id i_lhs lhs_i in
					      Agent.fold_interface
						(fun site (inf,lnk) set ->
						   let set =
						     match inf with
							 Agent.Wildcard -> set
						       | _ -> PortSet.add (j_sol,site^"~") set
						   in
						     match lnk with
							 Agent.Wildcard -> set
						       | _ -> PortSet.add (j_sol,site^"!") set
						) ag_i set
					  with Not_found -> 
					    let s="Simulation.update: assoc_lhs_sol invariant violation" in
					      Error.runtime
						(Some "simulation2.ml",
						 Some 1631,
						 Some s)
						s
				       ) assoc_lhs_sol PortSet.empty
				   in
				   let contains_modif = 
				     (*if not warn then true 
				       else*) 
				     try PortSet.fold (fun q b -> 
							 if PortSet.mem q upd_quarks then raise True
							 else b
						      ) quarks false 
				     with True -> true
				   in
				     if not contains_modif then 
				       begin
					 if !debug_mode then Printf.printf "but it is not using any modified quark\n" ;
					 assoc_list
				       end
				     else
				       (rule_ind,ind_cc,assoc_lhs_sol,quarks)::assoc_list
				) assoc_map_lhs_sol assoc_list
			  with Solution.Matching_failed -> assoc_list
		       ) r_i.lhs assoc_list
      ) next []
  in
  let _ = Implementation_choices.Clean_solution.clean_solution solution_AA_create  in
  let injections,lift,rules,mod_obs,inf_list =
    List.fold_right (fun (r_i,ind_cc,assoc,quarks) (injs,lift,rules,mod_obs,inf_list) -> 
		       
		       let free_keys,fresh,assoc_map = 
			 try InjArray.find (Coord.of_pair (r_i,ind_cc)) injs 
			 with Not_found -> ([],0,(*IntMap.empty*) AssocArray.create 1)
		       in
		       let new_key,fresh',free_keys' = 
			 match free_keys with
			     [] -> (fresh,fresh+1,[])
			   | h::tl -> (h,fresh,tl)
		       in
		       let injs = 
			 if !debug_mode then 
			   if (consistency_check assoc assoc_map) then ()
			   else (
			     Printf.printf "****Erreur: CC[%d,%d]****\n" r_i ind_cc ;
			     Printf.printf "%s\n" (string_of_set string_of_port PortSet.fold upd_quarks) 
			   ) ;
			 InjArray.add 
			   (Coord.of_pair (r_i,ind_cc)) 
			   (free_keys',fresh', AssocArray.add new_key assoc assoc_map) injs
			   
		       and lift =
			 PortSet.fold (fun q lift -> 
					 let cset = 
					   try (*PortMap.find q lift with Not_found -> CoordSet.empty in*)
					     CoordSetArray.find q lift 
					   with Not_found -> CoordSet.empty 
					 in
					   (*PortMap.add q (CoordSet.add [r_i;ind_cc;new_key] cset) lift*)
					   CoordSetArray.add q (CoordSet.add (Coord.of_pair (r_i,ind_cc),new_key) cset) lift
				      ) quarks lift
		       in
		       let rules,mod_obs,inf_list = 
			 try
			   let t0 = chrono 0.0 in
			   let r,inst_r = Rule_of_int.find r_i rules in
			   let _ = if !bench_mode then Bench.t_upd_rules:=!Bench.t_upd_rules +. (chrono t0) in
			   let inst_r' = IntMap.fold (fun cc_i _ act -> 
							let _,_,map = 
							  try InjArray.find (Coord.of_pair (r_i,cc_i)) injs 
							  with Not_found -> 
							    ([],0, AssocArray.create 1)
							in
							  (float_of_int (AssocArray.size map)) *. act
						     ) r.lhs 1.0
			   in
			   let mod_obs = 
			     if IntSet.mem r_i sim_data.obs_ind then IntSet.add r_i mod_obs
		     	     else mod_obs
			   in
			     (*boosting rule kinetics if necessary*)
			   let boost = 
			     let phi = 
			       try
				 let _,_,map0 = InjArray.find (Coord.of_pair (r_i,0)) injs
				 in
				   float_of_int (AssocArray.size map0)
			       with Not_found -> 0.0 (*cc has no injection*)
			     in
			     let psi = 
			       try
				 let _,_,map1 = InjArray.find (Coord.of_pair (r_i,1)) injs
				 in
				   float_of_int (AssocArray.size map1)
			       with Not_found -> 0.0 (*cc has no injection or unary rule*)
			     in
			       match r.intra with 
				   None -> r.kinetics
				 | Some u -> 
				     if phi = 0. or psi = 0. then r.kinetics (*no need to boost for binary rule has no instance, which means unary one has no instance either*)
				     else
				       max (u /. (phi +. psi)) (r.kinetics) 
			   in
			     
			   let t0 = chrono 0.0 in
			   let rules = Rule_of_int.add r_i ({r with boost = boost},inst_r') rules in
			   let inf_list = (*adding infinite rate rule with new instances*)
			     if ((int_of_float inst_r') > 0) && (IntSet.mem r_i sim_data.oo) then IntSet.add r_i inf_list
			     else inf_list
			   in
			   let _ = if !bench_mode then Bench.t_upd_rules:=!Bench.t_upd_rules +. (chrono t0) in
			     (rules,mod_obs,inf_list)
			 with Not_found -> 
			   let s = "Simulation.update: invalid rule indice" in
			     Error.runtime
			       (Some "simulation2.ml",
				Some 1755,
				Some s)
			       s
		       in
			 (injs,lift,rules,mod_obs,inf_list)
		    ) assoc_list (injs,lift,rules,mod_obs,inf_list)
  in
  let t0 = chrono 0.0 in
  let r,_ = Rule_of_int.find r_ind sim_data.rules in
  let _ = if !bench_mode then Bench.t_upd_rules:=!Bench.t_upd_rules +. (chrono t0) in
  let corr = r.corr_ag in
  let sim_data =
    {sim_data with 
       lift=lift; 
       injections=injections; 
       sol=sol ;
       n_ag = sim_data.n_ag + corr ; 
       rules = Rule_of_int.restore_consistency rules;
       inf_list=inf_list
    }
  in
  let lab = sim_data.lab in
  let sim_data = 
    IntSet.fold (fun obs_ind sim_data ->
		   try
		     let r,_ = Rule_of_int.find obs_ind sim_data.rules in
		     let flag = Rule.name r in
		     let indices_pert = StringMap.find flag lab.name_dep in
		     let sim_data,perts,indices = 
		       IntSet.fold (fun i (sim_data,perts,indices) ->
				      try
					let pert_i = IntMap.find i lab.perturbations in
					let do_apply = List.for_all (fun test -> test (sim_data.rule_of_name,sim_data.rules)) pert_i.test_list in
					  if not do_apply then (sim_data,perts,indices)
					  else
					    (
					      if !debug_mode then 
						Printf.printf "Applying %s\n" (string_of_perturbation pert_i) ;
					      let (oo,inf_list,rules) = 
						pert_i.modif (sim_data.oo,sim_data.inf_list,sim_data.rule_of_name,sim_data.rules) 
					      and perts = IntMap.remove i perts
					      and indices = IntSet.remove i indices 
					      in
						if !debug_mode then (
						  print_string "**********\n";
						  print_rules rules sim_data.inf_list;
						  print_string "**********\n"
						) ;
						({sim_data with rules = rules ; oo=oo ; inf_list = inf_list},perts,indices)
					    )
				      with
					  Not_found -> 
					    let perts = IntMap.remove i perts
					    and indices = IntSet.remove i indices
					    in
					      (sim_data,perts,indices)
				   ) indices_pert (sim_data,lab.perturbations,indices_pert)
		     in
		     let name_dep = 
		       if IntSet.is_empty indices then StringMap.remove flag lab.name_dep
		       else StringMap.add flag indices lab.name_dep
		     in
		       {sim_data with lab = {lab with perturbations = perts ; name_dep = name_dep}}
		   with Not_found -> sim_data (*perturbation not depending on the observable*)
		) mod_obs sim_data
  in
  let _ = if !bench_mode then Bench.pos_upd := !Bench.pos_upd +. (chrono t_pos) in
    (sim_data,mod_obs)

let ticking c = 
  let p = float_of_int !clock_precision 
  and mx_i = float_of_int !max_iter
  and mx_e = float_of_int !max_step
  and mx_t = !max_time
  and x_e = float_of_int c.curr_step 
  and x_t = c.curr_time
    (* and x_i = float_of_int c.curr_iteration*)
  in
  let event_tick = if (mx_e>0.0) then  x_e/.mx_e else 0.0
  and time_tick = if (mx_t>0.0) then x_t/.mx_t else 0.0
  in
  let y = int_of_float ((p/.mx_i) *. (max event_tick time_tick))
  and x = int_of_float (p/.mx_i)
  in
  let n = if c.restart then max 0 (x - c.curr_tick) else max 0 (y - c.curr_tick) in
  let rec display t = 
    if t<0 then failwith "Simulation2.ticking: negative value"
    else
      if t=0 then flush stdout 
      else
	(print_string Data.tick_string ; display (t-1))
  in
    display n ;
    {c with curr_tick = c.curr_tick + n}
      
(********************************************************************************************************)
(***********************************************EVENT LOOP***********************************************)
(********************************************************************************************************)

let event log sim_data p c story_mode =
  if !debug_mode then Printf.printf "%d:(%d,%f)\n" c.curr_iteration c.curr_step c.curr_time ; flush stdout;
  let stop_test curr_step curr_time  = 
    ((!max_time > 0.0) && (curr_time > !max_time)) or ((!max_step>0) && (curr_step > !max_step))
  in
    
  let t_select = chrono 0.0 in
  let (log,inj_list,sim_data,clashes) = select log sim_data p c in
    if !bench_mode then Bench.rule_select_time := !Bench.rule_select_time +. (chrono t_select) ;
    match inj_list with
	None -> 
	  let log = Session.add_log_entry 1 
	    (Printf.sprintf "Deadlock found (activity = %f)" (Rule_of_int.accval sim_data.rules)) log
	  in
	    (log,sim_data,p,{c with deadlock = true}) 
      | Some (abst_ind,r_ind,assoc_list) ->
	  let activity = Rule_of_int.accval sim_data.rules in
	  let dt = 
	    if !Data.no_random_time then 1./. activity (*expectency*)
	    else Mods2.random_time_advance activity clashes 
	  in (*sums clashes+1 time advance according to activity*)
	  	    
	  (**********************************************************************************************)
	  (*******************************************MEASUREMENTS***************************************)
	  (**********************************************************************************************)
	    
	  let c = 
	    if story_mode or !ignore_obs or (!init_time > c.curr_time) then c 
	    else
	      let k = match c.points with
		  (k,_,_)::_ -> k
		| [] -> 0
	      in
	      let take_measures,measurement_t = 
		match c.measure_interval with
		    DE delta_e -> ((delta_e * k) <= c.curr_step,c.curr_time)
		  | Dt delta_t -> let t = delta_t *. (float_of_int k) in (t <= c.curr_time, t +. !init_time)
		  | _ -> raise (Error.Runtime "Simulation2.event invalid time or event increment")
	      in
		if not take_measures then c
		else
		  let _ = () 
		    (*Printf.printf "Measure : %d,%f\n" (k+1) measurement_t ; flush stdout *)
		  in
		  let obs_list = 
		    IntSet.fold (fun ind_obs cont ->
				   let r_obs,inst_obs = Rule_of_int.find ind_obs sim_data.rules in
				     if r_obs.input = "var" then cont
				     else
				       let automorphisms = 
					 match r_obs.automorphisms with 
					     None -> (failwith "Automorphisms not computed") 
					   | Some i -> float_of_int i 
				       in
				       let act_obs = (inst_obs *. r_obs.kinetics) /. automorphisms
				       in 
					 (string_of_int (int_of_float act_obs))::cont
				) sim_data.obs_ind []
		  in
		    {c with points = (k+1,measurement_t,obs_list)::c.points ; last_k = k}
	  in

	  let c = 
	    if IntSet.mem r_ind sim_data.oo then c 
	    else
	      {c with curr_time = c.curr_time +. dt  ; curr_step = c.curr_step+1}
	  in

	  (**********************************************************************************************)
	  (***************************************RULE APPLICATION***************************************)
	  (**********************************************************************************************)
	    

	  let r_abst,_ = Rule_of_int.find abst_ind sim_data.rules in
	  let r_ref,_ = Rule_of_int.find r_ind sim_data.rules in
	  let _ =
	    if !debug_mode then 
	      begin
		Printf.printf "%f,r[%d]: %s\n" c.curr_time r_ind (Rule.name r_ref); flush stdout;
		Printf.printf "INF: %s\n" (string_of_set string_of_int IntSet.fold sim_data.inf_list) ;
	      end
	  in
	  let t_apply = chrono 0.0 in
	  let (mq,rmq,tq,assoc_add,sol',warn) = Rule.apply r_abst assoc_list sim_data.sol in (*passing r_abst in order to have a mq,rmq,tq as small as possible*)
	  let _ = if !bench_mode then Bench.rule_apply_time := !Bench.rule_apply_time +. (chrono t_apply) in
	  let log = 
	    if warn then
	      Session.add_log_entry 4 ("Application of rule ["^(Rule.name r_abst)^"] was contextual") log  (*application of r_abst might be contextual when of r_ref would not*)
	    else log
	  in
	    (*merge modif quarks and removed quarks*)
	  let upd_q = PortMap.fold (fun q _ pset -> PortSet.add q pset) mq rmq in
	  let t_update = chrono 0.0 in
	  let assoc = 
	    match assoc_list with
		[phi] -> phi
	      | [phi1;phi2] -> 
		  let union,_ = IntMap.fold (fun i j (union,image) ->
					       if IntSet.mem j image then let s = "Simulation2.iter: intras are clashing!" in
						 Error.runtime 
						   (Some "simulation2.ml",
						    Some 1980,
						    Some s)
						   s
					       else
						 (IntMap.add i j union,IntSet.add j image)
					    ) phi1 (phi2,IntSet.empty)
		  in
		    union
	      | _ -> let s = "Simulation2.iter: invalid number of intras" in
		  Error.runtime 
		    (Some "simulation2.ml",
		     Some 1991,
		     Some s)
		    s
	  in
	  let sim_data,mod_obs = update warn abst_ind assoc upd_q assoc_add sol' sim_data p c (*update abst_ind is equal to original rule*)
	  in
	  let _ = if !bench_mode then Bench.update_time := !Bench.update_time +. (chrono t_update) in

	    (********************************************************************************************************************)
	    (***********************************************CAUSALITY ANALYSIS MODE**********************************************)
	    (********************************************************************************************************************)	    
	    if story_mode then 
	      let net',modifs = 
		let modifs = PortMap.fold (fun quark test_modif pmap ->
					     PortMap.add quark test_modif pmap
					  ) tq mq
		in
		let modifs = PortSet.fold (fun quark pmap ->
					     let old = 
					       try 
						 PortMap.find quark pmap 
					       with 
						   Not_found -> [] 
					     in
					       PortMap.add quark (Rule.Remove::old) pmap
					  ) rmq modifs 
		in 
		  if IntSet.mem r_ind sim_data.incompressible then (*rule is to be observed, so don't backtrack it!*)
		    (Network.add sol' sim_data.net (r_ref,modifs) !debug_mode false, modifs)
		  else
		    (Network.add sol' sim_data.net (r_abst,modifs) !debug_mode p.compress_mode, modifs)
	      in
	      let sim_data = {sim_data with sol = sol' ; net = net'} in

		if (IntSet.mem r_ind sim_data.obs_ind) then (*if applied rule triggers storification*)
		  
		  let flg = match r_ref.flag with Some flg -> flg | _ -> 
		    let s = "Simulation.iter: obs has no flag" in
		      runtime
			(Some "simulation2.ml",
			 Some 2020,
			 Some s)
			s
		  in
		  let roots = Rule.roots_for_story r_ref in
		  let h_opt = Network.cut net' (roots,flg) in
		    match h_opt with
			None -> 
			  let limit_reached = stop_test c.curr_step c.curr_time in
			    (log,sim_data,p,{c with 
					       curr_iteration = if limit_reached then c.curr_iteration + 1 else c.curr_iteration ;
					       restart = limit_reached
					    }
			    ) 
		      | Some h ->
			  begin
			    if !debug_mode then print_string "Causal trace found, checking whether constraints are satisfied\n" ; flush stdout ;
			    let h = 
			      {h with 
				 Network.name_of_agent = 
				  let n = Solution.AA.size sol'.Solution.agents in
				  let vect = Array.make n "" in
				  let rec aux k = 
				    if k = n then ()
				    else 
				      ((vect.(k) <- 
					  try 
					    (let ag = 
					       Solution.AA.find k sol'.Solution.agents in Agent.name ag) with _ -> "");
				       aux (k+1))
				  in
				  let _ = aux 0 in
				    Some vect} 
			    in
			    let opt = Story_compressor.compress h !Data.story_compression_mode Data.WEAK in
			      match opt with
				  None -> 
				    let log = Session.add_log_entry 4 "-Causal trace does not satisfy constraints after compression" log in
				    let limit_reached = stop_test c.curr_step c.curr_time in
				      (log,sim_data,p,{c with 
							 curr_iteration = if limit_reached then c.curr_iteration + 1 else c.curr_iteration ;
							 restart = limit_reached
						      }
				      ) 
				| Some compressed_h ->
				    let drawers = (Iso.classify (compressed_h,c.curr_time) c.drawers p.iso_mode) 
				    in
				    let log = Session.add_log_entry 4 "-Causal trace found!" log in
				      (log,sim_data,p,{c with 
							 curr_iteration = c.curr_iteration+1 ; 
							 drawers = drawers ; 
							 restart = true
						      }) 
			  end
		else (*last rule was not observable for stories*)
		  let limit_reached = stop_test c.curr_step c.curr_time in
		    (log,sim_data,p,{c with 
				       curr_iteration = if limit_reached then c.curr_iteration + 1 else c.curr_iteration ;
				       restart = limit_reached
				    }
		    ) 
		      
	    else 
	      (log,{sim_data with sol = sol'},p,c)
