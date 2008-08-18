open Network
open Mods2
open Tools

let debug = false

module Iso = 
  struct
    type t = {phi:int IntMap.t;phi_m:int IntMap.t}
    exception Not_in_iso
    let add i j iso = 
      let phi = IntMap.add i j iso.phi
      and phi_m = IntMap.add j i iso.phi_m
      in
	{phi=phi;phi_m=phi_m}
    let empty = {phi=IntMap.empty;phi_m=IntMap.empty}

    let remove i iso = 
      try
	let j = IntMap.find i iso.phi in
	let phi = IntMap.remove i iso.phi 
	and phi_m = IntMap.remove j iso.phi_m
	in
	  {phi=phi;phi_m=phi_m}
      with 
	  Not_found -> raise Not_in_iso

    let image i iso = try IntMap.find i iso.phi with Not_found -> raise Not_in_iso
    let domain i iso = try IntMap.find i iso.phi_m with Not_found -> raise Not_in_iso
    let in_image i iso = IntMap.mem i iso.phi_m
    let in_domain i iso = IntMap.mem i iso.phi
    let mem i j iso = 
      let j' = image i iso
      and i' = domain j iso 
      in
	(i=i')&&(j=j')      
    let str iso =
      Printf.sprintf "(%s)" 
	(String.concat ","
	   (IntMap.fold (fun i j cont -> let s = Printf.sprintf "%d<->%d" i j in s::cont) iso.phi []))
  end

let sort net =
  let map_d =
    (*IntMap.fold*)
    EventArray.fold (fun eid e map_d ->
		       if e.kind =0 then map_d else
			 let map_e = try IntMap.find e.s_depth map_d with Not_found -> IntMap.empty in
			   IntMap.add e.s_depth (IntMap.add eid e map_e) map_d
		    ) net.events IntMap.empty
  in
    map_d

exception Failed

module Sign =
struct
  type t = (int*string*int) list (*depth,label,number*)

  let make net =
    let map =
      EventArray.fold (fun i e depth_map ->
			 if e.kind = 0 then depth_map else
			   let d = e.s_depth in
			   let lab_map = try Mods2.IntMap.find d depth_map with Not_found -> Mods2.StringMap.empty in
			   let freq = try Mods2.StringMap.find e.r.Rule.input lab_map with Not_found -> 0 in
			     Mods2.IntMap.add d (Mods2.StringMap.add e.r.Rule.input (freq+1) lab_map) depth_map
		      ) net.events Mods2.IntMap.empty
    in
      Mods2.IntMap.fold 
	(fun d lab_map cont -> (*d in increasing order*)
	   let l = Mods2.StringMap.fold (fun lab freq cont -> (*lab in increasing order*)
					  (d,lab,freq)::cont
				       ) lab_map [] 
	   in
	     l@cont
	) map []
  let str sgn =
    String.concat "\n"
      (List.map (fun (d,lab,n) -> Printf.sprintf "(%d,%s,%d)" d lab n) sgn)
      
end

type drawers = {hsh:(Sign.t,(t*int*float) list) Hashtbl.t; nb:int;  (*hsh: sg_i R (net_i*freq_i)*)} 
let empty_drawers n = {hsh=Hashtbl.create n; nb=0}

(*let cpt = ref 1 *)

let iso x y = true (*to be implemented*)

let classify (net,time) drawers test_iso_mode =
  let key_sgn = Sign.make net (*[]*) in
    try
      let nets_nb = Hashtbl.find drawers.hsh key_sgn 
      in
      let nets_nb = 
	let is_new,nets_nb' =
	  List.fold_right (fun (net',i,t) (is_new,cont) -> 
			     let isomorphic = 
			       if not test_iso_mode then true else (iso net net') 
			     in
			       if isomorphic then (false,(net',i+1,t+.time)::cont)
			       else (is_new,(net',i,t)::cont)
			  ) nets_nb (true,[])
	in
	  if is_new then ((net,1,time)::nets_nb') else nets_nb'
      in
	Hashtbl.replace drawers.hsh key_sgn nets_nb ;
	{drawers with nb = drawers.nb+1}
    with Not_found -> 
      Hashtbl.replace drawers.hsh key_sgn [(net,1,time)] ;
      {hsh=drawers.hsh; nb=(drawers.nb+1)}
	
let compress_drawers log drawers test_iso_mode add_log_entry = 
  let pt = Unix.times() in
  let time = pt.Unix.tms_utime +. pt.Unix.tms_stime +. pt.Unix.tms_cutime +. pt.Unix.tms_cstime in
    if !Data.story_compression then
      let k = 
	Hashtbl.fold 
	  (fun h l k ->
	     List.fold_right 
	       (fun 
		  (net',i,t) 
		  k -> 
		    (k+1))

	       l k)
	  drawers.hsh 0 in
	if k>0 then 
	  let log = add_log_entry 0 "--Story compression..." log in
	  let drawers' = empty_drawers 10 in 
	  let counter = init_counters () in 
	  let log,(rep,kold,knew,kfail,tot,counter),list  = 
	    Hashtbl.fold 
	      (fun h l (log,(drawers',kold,knew,kfail,tot,counter),list) ->
		 List.fold_right 
		   (fun (net',i,t) (log,(drawers',kold,knew,kfail,tot,counter),list) -> 
		      let _ = Story_compressor.set_init_time () in
		      let kold = kold+1 in
		      let net'',log  = 
			Story_compressor.compress 
			  (if Data.ignore_story_compression or !Data.log_compression
			   then Network.copy net'
			   else net')  
			  (!Data.story_compression_mode) 
			  Data.WEAK 
			  log
			  add_log_entry
		      in 
		      let net_weak = 
			if !Data.log_compression 
			then 
			  (match net'' with Some net'' -> (Network.copy net'') | _ -> net')
			else 
			  (match net'' with Some net'' -> net'' | _ -> net') 
		      in
		      let net_strong,log = 
			if (!Data.log_compression && !Data.log_strong_compression)
			  or (!Data.strong_compression) 
			then 
			  let net' = 
			    match net'' with Some net -> net | _ -> net' in
			  let net'' = 
			    let rec aux net k =
			      if not (Story_compressor.test_time ())
			      then 
				net
			      else
				
				let nevents = net.Network.fresh_id in 
				let nevent = Network.weigth net in 
				let _ = 
				  if false then 
				    let _ = print_int k in 
				    let _ = print_string "/" in
				    let _ = print_int nevents in
				    let _ = print_newline () in
				      ()
				in
				  if k>=nevents then net 
				    
				  else
				    let perm_list = Story_compressor.Convert.try_permutation net k in
				    let rec aux2 l = 
				      if not (Story_compressor.test_time ()) then net
				      else 
					match l with 
					    [] -> aux net (k+1)
					  | (Some t)::q -> 
					      (match 
						 (try (Story_compressor.compress 
							 t (!Data.story_compression_mode)
							 (Data.WEAK) log add_log_entry)
						  with _ -> (*print_string "BUG: ISO 189 arg should be s valid story";print_newline ();*)None,log)
					       with Some net',_ ->
						 let nevent' = Network.weigth net' in
						 let nevents' = net'.Network.fresh_id in
						   if (
						     if !Data.use_multiset_in_strong_compression then compare_net nevent' nevent < 0  
						     else nevents' < nevents
						   ) then aux net' 0 
						   else aux2 q
						 | _ -> aux2 q)
					  | None::q -> aux2 q 
				    in aux2 perm_list
			    in 
			      aux net' 0 in 
			    net'',log 
			else net',log in 
		      let net'' = 
			if Data.ignore_story_compression
			then Some net' else 
			  if !Data.strong_compression    then Some net_strong 
			  else Some net_weak in  
			match net'' with 
			    None ->   
			      let vclock = (kold*(!Data.clock_precision))/k in
			      let counter = ticking vclock counter in
				log,(drawers',kold,knew,kfail+1,tot,counter),(net',net_weak,net_strong,i,t)::list
			  |	Some net'' -> 
				  let vclock = (kold*(!Data.clock_precision))/k in
				  let counter = ticking vclock counter in
				  let key_sgn = Sign.make net'' in 
				    log,(try
					   let nets_nb = Hashtbl.find drawers'.hsh key_sgn in
					   let nets_nb,kold,knew = 
					     let is_new,nets_nb' =
					       List.fold_right (fun (net',i',t') (is_new,cont) -> 
								  let isomorphic = 
								    if not test_iso_mode then true else (iso net'' net') 
								  in
								    if isomorphic then (false,(net',i+i',t+.t')::cont)
								    else (is_new,(net',i',t')::cont))
						 nets_nb (true,[])
					     in
					       if is_new then 
						 ((net'',i,t)::nets_nb',kold,knew+1) else 
						   nets_nb',kold,knew
					   in
					     Hashtbl.replace drawers'.hsh key_sgn nets_nb ;
					     {drawers' with nb = drawers'.nb},kold,knew,kfail,tot+i,counter
					 with Not_found -> 
					   Hashtbl.replace drawers'.hsh key_sgn [(net'',i,t)] ;
					   ({hsh=drawers'.hsh; 
					     nb=(drawers'.nb)},
					    kold,knew+1,kfail,tot+i,counter 
					   )),
				  if !Data.log_compression 
				  then (net',net_weak,net_strong,i,t)::list
				  else list)
		   l (log,(drawers',kold,knew,kfail,tot,counter),list)   
	      ) 
	      drawers.hsh 
	      (log,(drawers',0,0,0,0,counter),[]) in
	  let drawers' = {drawers' with nb = tot} in 
	  let _ = Printf.fprintf stderr "\n" in 
	  let log = add_log_entry 0 ("--compression "^(string_of_int kold)^" into "^(string_of_int knew)) log in 
	  let log = 
	    if  kfail > 0 
	    then 
	      add_log_entry 0 ("   "^(string_of_int kfail)^" failure") log 
	    else
	      log in 
	  let pt = Unix.times() in
	  let t' = pt.Unix.tms_utime +. pt.Unix.tms_stime +. pt.Unix.tms_cutime +. pt.Unix.tms_cstime in
	  let log = add_log_entry 0 (Printf.sprintf "--Story compression: %f sec. CPU" (t'-.time))  log in
	    log,drawers',list
	else
	  log,drawers,[]
    else
      log,drawers,[]

