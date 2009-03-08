open Data_structures_metaplx 

let trace = false
let trace_print = if trace then print_string else (fun _ ->())

let get_instr l = 
  List.fold_left 
    (fun liste a ->
      Agent_interfaces.convert_action a liste)
    [] 
    l

let get_interface parsed_agent = 
  match parsed_agent with 
    (a,b) -> (a,List.fold_left (fun set (a,_) -> SiteSet.add a set) SiteSet.empty b)
      
let add_concrete a b subs = 
  try 
    if SiteSet.equal 
	(match AgentMap.find a subs.concrete_names
	with Some a,_ -> a | _ -> raise Not_found)
	b 
    then subs
    else
      failwith "add_concrete"
  with 
    Not_found -> 
      {subs with concrete_names = AgentMap.add a (Some b,[]) subs.concrete_names}

let add_gen (a,b,c,d) subs = 
  let agent = 
    match a,b with _,Some e -> e
    | Some ((a,_),_),_-> a in
  try 
    let _ = AgentMap.find agent subs.definitions in failwith "add_gen" 
  with 
    Not_found -> 
      let def = 
	match (a,b,c,d) with 
	  Some ((a,b),_),_,_,_ -> 
	    let _ = trace_print "root\n" in
	    Root(List.fold_left (fun sol (a,_) -> SiteSet.add a sol) SiteSet.empty b),None 
	| _,_,Some agent',list -> 
	    let _ = trace_print "variant\n" in 
	    Variant(agent',get_instr list),None 
      in
      {subs with definitions = 
	AgentMap.add agent def  subs.definitions}


let add_conc (a,b,c,d) subs = 
  let subs = add_gen (a,b,c,d) subs in 
  match a,b with 
    Some ((a,b),_),_ -> 
      begin
	let interface = List.fold_left (fun sol (a,_) -> SiteSet.add a sol) SiteSet.empty b in 
	let _ = trace_print "CONC\n" in 
	add_concrete a interface subs 
      end
  | _, Some e -> 
      begin
	try 
	  let _ = AgentMap.find e subs.concrete_names in subs 
	with 
	  Not_found -> 
	    let _ = trace_print "NEWCONC\n" in 
	  {subs with concrete_names = AgentMap.add e (None,[]) subs.concrete_names}
      end
 
let convert lines = 
  List.fold_left 
    (fun 
      sol x -> 
	match x 
	with 
	  INIT_L (a,_) -> 
	    List.fold_left 
	      (fun sol a -> 
		let ag,site = get_interface a in 
		add_concrete ag site sol)
	      sol a 
	| DONT_CARE_L _ -> sol 
	| GEN_L p -> add_gen p sol
	| CONC_L p -> add_conc p sol 
	| RULE_L _ -> sol 
	| PREPROCESSED_RULE _ -> sol
	| OBS_L _ | STORY_L _ -> sol )
    { concrete_names=AgentMap.empty;
      definitions=AgentMap.empty}
    lines 

