open Simulation2
open Experiment
open Mods2
open Error
open Data
open Error_handler_common
open Error_handler 

let print_task_list task_list = 
  let string_of_task tsk = 
    match tsk with
	SAVE_MIXTURE -> "save M"
      | SAVE_STATE -> "save S"
      | CANCEL_PERTURBATION i -> ("cancelling perturbation "^(string_of_int i))
      | ACTIVATE_PERTURBATION i -> ("activate perturbation "^(string_of_int i))
      | TAKE_SNAPSHOT -> "snapshot"
  in
    List.iter (fun (t,tsk) -> Printf.printf "(%f,%s)" t (string_of_task tsk)) task_list ;
    print_newline()

let rec apply sd c p log finalize =
  match sd.task_list with
      (t,tsk)::tl -> 
	if t >= c.curr_time then 
	  if c.deadlock then 
	    apply sd {c with curr_time = t ; deadlock = false} p 
	      (Session.add_log_entry 1 (Printf.sprintf "-Task list not empty, moving time to %f" t) log) finalize
	  else
	    ({sd with task_list = ((t,tsk)::tl)},c,p,log)
	else
	  begin
	    match tsk with
		SAVE_MIXTURE -> 
		  let log = Session.add_log_entry 4 (Printf.sprintf "-Saving mixture at time %f as required..." t) log
		  in
		  let species = Species.of_sol sd.sol in
		  let f_init = 
		    Species.fold (fun _ list cont ->
				    List.fold_left (fun cont (spec,n) -> (Solution.marshal spec.Species.sol,n)::cont) cont list
				 ) species []
		  in
		  let file = !serialized_mixture_file^(string_of_float t) in
		  let d = open_out_bin file in
		    Marshal.to_channel d f_init [] ;
		    close_out d ;
		    apply {sd with task_list = tl} c p log finalize

	      | SAVE_STATE ->
		  let log = Session.add_log_entry 4 (Printf.sprintf "-Saving simulation state at time %f as required..." t) log
		  in
		  let f_sd = Simulation2.marshal sd in
		  let file = if t>0.0 then !serialized_sim_data_file^(string_of_float t) else !serialized_sim_data_file in
		  let p =
		    if (!max_iter >1) then (
		      Data.tmp_file := file::(!Data.tmp_file) ;
		      {p with init_sd = Some file}
		    )
		    else p
		  in
		  let d = open_out_bin file in
		    Marshal.to_channel d (c.curr_time,f_sd) [] ;
		    close_out d ;
		    apply {sd with task_list = tl} c p log finalize
		      
	      | CANCEL_PERTURBATION i ->
		  if finalize then apply {sd with task_list = tl} c p log finalize
		  else
		    begin
		      let opt = try Some (IntMap.find i sd.lab.perturbations) with Not_found -> None
		      in
			match opt with
			    None -> apply {sd with task_list = tl} c p log finalize
			  | Some pert_i ->
			      let log = Session.add_log_entry 4 (Printf.sprintf "-Removing perturbation %s..." (string_of_perturbation pert_i)) log
			      in
			      let lab = {sd.lab with perturbations = IntMap.remove i sd.lab.perturbations }  
			      in
				apply {sd with lab = lab ; task_list = tl} c p log finalize
		    end

	      | ACTIVATE_PERTURBATION i ->
		  if finalize then apply {sd with task_list = tl} c p log finalize
		  else
		    begin
		      let opt = try Some (IntMap.find i sd.lab.perturbations) with Not_found -> None 
		      in
			match opt with
			    None -> apply {sd with task_list = tl} c p log finalize (*perturbation already removed*)
			  | Some pert_i ->
			      let do_apply = List.for_all (fun test -> test (sd.rule_of_name,sd.rules)) pert_i.test_list in
			      let sd,log = 
				if not do_apply then 
				  let name_dep = (*adding concentration dependencies for pert_i to the lab for future wake-up*)
				    List.fold_left (fun name_dep dep ->
						      match dep with
							  GREATER_TIME _ -> name_dep (*ignoring time dependencies now*)
							| LESSER_TIME _ -> name_dep
							| RULE_FLAGS l -> 
							    List.fold_right (fun flg map -> 
									       let set = try StringMap.find flg map with Not_found -> IntSet.empty in
										 StringMap.add flg (IntSet.add i set) map
									    ) l name_dep
						   ) sd.lab.name_dep pert_i.dep_list
				  in
				  let lab = {sd.lab with name_dep = name_dep}
				  in
				    ({sd with lab = lab},log)
				else (*apply perturbation directly*)
				  let log = Session.add_log_entry 4
				    (Printf.sprintf "-Applying perturbation %s (iteration %d, time %f)..." (string_of_perturbation pert_i) c.curr_iteration c.curr_time) log
				  in
				    begin
				      let (oo,inf_list,rules) = pert_i.modif (sd.oo,sd.inf_list,sd.rule_of_name,sd.rules) in
				      let lab = {sd.lab with perturbations = IntMap.remove i sd.lab.perturbations}
				      in
					({sd with rules = rules ; lab = lab ; oo = oo ; inf_list = inf_list},log)
				    end
			      in
				apply {sd with task_list = tl} c p log finalize
		    end
		      
	      | TAKE_SNAPSHOT -> 
		  let log =
		    (
		      Session.snapshot (sd.sol,c.curr_time,c.snapshot_counter) ;
		      Session.add_log_entry 4 (Printf.sprintf "Taking snapshot at t=%f" c.curr_time) log 
		    )
		  in
		    apply {sd with task_list = tl} {c with snapshot_counter = c.snapshot_counter+1} p log finalize
		      
	  end
    | [] -> (sd,c,p,log)



