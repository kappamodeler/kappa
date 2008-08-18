open Mods2

module LSMap = Map_ext.Make(struct type t = LongString.t let compare = compare end) 

type spec = {signature:LongString.t; sol:Solution.t; matching:(int * Solution.cc_recognition * int IntMap.t) list}

(*cc: connected component*)
let spec_of_cc cc = 
  let get_sig cc = 
    let l = 
      List.fast_sort compare
	(Solution.AA.fold (fun id ag l ->
			     (Agent.to_str ag)::l
			  ) cc.Solution.agents [])
    in
      List.fold_left (fun ls ag_str -> LongString.concat ag_str ls) LongString.empty l
  in
    {signature=get_sig cc ; sol = cc ; matching=Solution.recognitions_of_cc cc}

let iso spec1 spec2 = (*tests whether spec1 embeds in spec2*)
  let precomp = spec1.matching in
    try
      let _ = Solution.unify ~rooted:true (precomp,spec1.sol) (spec2.sol.Solution.agents,spec2.sol)
      in
	true
    with
	Solution.Matching_failed -> false
 
let of_sol sol = 
  let _,spec_map =
    Solution.AA.fold (fun id _ (blacklist,spec_map) -> 
			if IntSet.mem id blacklist then (blacklist,spec_map) 
			else
			  let sol_cc = Solution.cc_of_id id sol in
			  let blacklist = Solution.AA.fold (fun id _ set -> 
							      IntSet.add id set
							   ) sol_cc.Solution.agents blacklist
			  in
			  let new_spec = spec_of_cc sol_cc in
			  let spec_list = try LSMap.find new_spec.signature spec_map with Not_found -> [] in (*may raise Not_found*)
			  let spec_list = 
			    let spec_list',already_added = 
			      List.fold_left (fun (spec_list,already_added) (old_spec,n) -> 
						let is_equal = iso old_spec new_spec in
						  if already_added then ((old_spec,n)::spec_list,true) 
						  else 
						    if is_equal then ((old_spec,n+1)::spec_list,true)
						    else ((old_spec,n)::spec_list,false)
					     ) ([],false) spec_list 
			    in
			      if already_added then spec_list'
			      else (new_spec,1)::spec_list'
			  in
			    (blacklist,LSMap.add new_spec.signature spec_list spec_map) 
    		     ) sol.Solution.agents (IntSet.empty,LSMap.empty)
  in
    spec_map

let sol_of_spec spec = spec.sol
let kappa_of_spec = fun x -> Solution.kappa_of_solution (sol_of_spec x)

let fold = LSMap.fold
	
