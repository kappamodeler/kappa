open Error
open Mods2
open Solution

type t = {lhs:Solution.t IntMap.t;
	  rhs:Solution.t;
	  precompil: ((int * Solution.cc_recognition * int IntMap.t) list) IntMap.t ;
	  add: Agent.t IntMap.t ; (*pid in rhs that are added by the rule*)
	  actions: ((Solution.action * Solution.msg) list) IntMap.t ;
	  corr_ag:int ;
	  rate:int;
	  input:string;
	  flag:string option ;
	  constraints: Solution.constraints list ;
	  kinetics: float ;
	  automorphisms: int option ; 
	  n_cc: int ;
	  id:int ;
	  infinite:bool ;
	  abstraction:t list option
	 }

type rule_type = t (*alias*)

type marshalized_t = {f_lhs:Solution.marshalized_t IntMap.t;
		      f_rhs:Solution.marshalized_t;
		      f_precompil: ((int * Solution.cc_recognition * int IntMap.t) list) IntMap.t ;
		      f_add: Agent.t IntMap.t ; (*pid in rhs that are added by the rule*)
		      f_actions: ((Solution.action * Solution.msg) list) IntMap.t ;
		      f_corr_ag:int ;
		      f_rate:int;
		      f_input:string;
		      f_flag:string option ;
		      f_constraints: Solution.constraints list ;
		      f_kinetics: float ;
		      f_automorphisms: int option ;
		      f_n_cc: int ;
		      f_id:int ;
		      f_infinite:bool ;
		      f_abstraction: marshalized_t list option
		     }

let rec marshal r = {f_lhs = IntMap.map Solution.marshal r.lhs ;
		 f_rhs = Solution.marshal r.rhs ;
		 f_precompil = r.precompil ;
		 f_add = r.add ;
		 f_actions = r.actions ;
		 f_corr_ag = r.corr_ag ;
		 f_rate = r.rate ;
		 f_input = r.input ;
		 f_flag = r.flag ;
		 f_constraints = r.constraints ;
		 f_kinetics = r.kinetics ;
		 f_automorphisms = r.automorphisms ; 
		 f_n_cc = r.n_cc ;
		 f_id = r.id ;
		 f_infinite = r.infinite ;
		 f_abstraction = 
		      match r.abstraction 
		      with None -> None
		      |	Some a -> Some (List.map marshal a) ; 
		}

let rec unmarshal f_r = {
                     lhs=IntMap.map Solution.unmarshal f_r.f_lhs ;
		     rhs=Solution.unmarshal f_r.f_rhs ;
		     precompil=f_r.f_precompil ;
		     add=f_r.f_add ;
		     actions=f_r.f_actions ;
		     corr_ag=f_r.f_corr_ag ;
		     rate=f_r.f_rate;
		     input=f_r.f_input;
		     flag=f_r.f_flag;
		     constraints=f_r.f_constraints;
		     kinetics=f_r.f_kinetics;
		     automorphisms=f_r.f_automorphisms;
                     n_cc=f_r.f_n_cc;
		     id=f_r.f_id;
		     infinite = f_r.f_infinite;
		     abstraction = 
		      match f_r.f_abstraction with 
			None -> None
		      |	Some a -> Some (List.map unmarshal a) ;
		    }


let empty = {lhs=IntMap.empty;
	     rhs=Solution.empty();
	     precompil=IntMap.empty;
	     actions=IntMap.empty;
	     corr_ag = 0;
	     add = IntMap.empty;
	     rate=0;
	     input="";
	     flag=None;
	     constraints=[];
	     kinetics = 1.0 ;
             automorphisms = None ;
	     n_cc = 0 ;
	     id = -1 ;
	     infinite = false ;
	     abstraction = None ;
    	    }


let name r = 
  match r.flag with
      None -> r.input
    | Some flg -> flg


(*Modified map to select a rule according to its activity in log time*)
module Rule_of_int = 
  (Implementation_choices.Rule_of_int 
    (struct type t = int 
      type map_val = (rule_type * float) 
      let def = (empty,0.0)
      let compare = compare 
     let to_f = 
       fun (r,inst) ->
	 if r.input = "" then 0.0 
	 else
	   let automorphisms = 
	     match r.automorphisms with 
		 None -> (failwith ("Rule_of_int: automorphisms not computed for rule "^r.input)) 
	       | Some i -> float_of_int i 
	   in
	   let k = r.kinetics in k *. inst /. automorphisms
    end):Val_map.S with type val_type = (rule_type * float) and type key = int)


let print_compil ?(filter=false) ?(with_warning=false) r = 
  let print_actions add rhs acts =
    let _ = 
      IntMap.fold (fun id ag _ -> 
		     Printf.printf "ADD %s#%d(%s)\n" (Agent.name ag) id 
		       (String.concat "," ((Agent.fold_interface (fun site (inf,_) l -> 
								    if site = "_" then l else
								    match inf with
									Agent.Marked s -> (site^"~"^s)::l
								      | _ -> site::l
								 ) ag [])
					  )
		       )
		  ) add ()
    in
      IntMap.iter 
	(fun id inst_msg_list  ->
	   let l =
	     List.fold_left
	       (fun cont (inst,msg) ->
		  let s_inst =
		    match inst with
			Solution.Bind (s,i,s') -> Printf.sprintf "BND (#%d,%s) (#%d,%s)"  id s i s'
		      | Solution.Break (s,i,s') -> Printf.sprintf "BRK (#%d,%s) (#%d,%s)"  id s i s'
		      | Solution.Modify s -> Printf.sprintf "BRK (#%d,%s)"  id s
		      | Solution.Mark (s,mk) -> Printf.sprintf "MOD (#%d,%s) with %s"  id s mk
		      | Solution.Remove -> Printf.sprintf "DEL #%d" id
		  and s_msg = 
		    match msg with
			Solution.OK -> ""
		      | Solution.Warning s -> if with_warning then Printf.sprintf "\nWarning %s"  s else ""
		  in
		    (Printf.sprintf "%s%s\n" s_inst s_msg)::cont
	       ) [] inst_msg_list
	   in
	     Printf.printf "%s" (String.concat "" l);
	     if r.infinite then Printf.printf "Infinitely fast\n"
	) acts 
  in

  let rec print_constraints constraints = 
    match constraints with
	CC(i,joints,disjoints,sep)::tl -> 
	  if IntSet.is_empty joints then ()
	  else
	    Printf.printf "Agents in %s are in CC(#%d)" 
	      (string_of_set (fun i ->"#"^(string_of_int i)) IntSet.fold joints) i  ;
	  if IntSet.is_empty disjoints then ()
	  else
	    Printf.printf "Agents in %s are not in CC(#%d)" 
	      (string_of_set (fun i ->"#"^(string_of_int i)) IntSet.fold disjoints) i  ;
	  if StringSet.is_empty sep then ()
	  else
	    Printf.printf "CC(#%d) does not contain agent types in %s"
	      i (string_of_set (fun i -> i) StringSet.fold sep)   ;
	  print_constraints tl
      | [] -> print_newline()
  in
  let print_rule() = 
    let flg = match r.flag with
	Some s -> s^": "
      | None -> ""
    in
      if r.rate = (-1) then print_string ("?"^r.input^"?\n") 
      else (
	if not filter or (r.rate > 1) then (
	  print_string (String.make (String.length r.input) '-') ;
	  print_newline();
	  print_string flg;
	  print_string r.input;print_newline();
	  print_string (String.make (String.length r.input) '-') ;
	  print_newline();
	  print_constraints r.constraints
	)
      )
  in
    if not filter or (r.rate > 1) then (
      print_actions r.add r.rhs r.actions ;
      print_rule();
      print_string "\n"
    )

let rhs r = r.rhs 

let flag r = r.flag

let mod_quarks r = 
  let pset,iset =
    IntMap.fold (fun id ag (pset,iset) ->
		   Agent.fold_interface 
		     (fun s _ (pset,iset) ->
			(PortSet.add (id,s^"!") (PortSet.add (id,s^"~") pset),
			 IntSet.add id iset) 
		     ) ag (pset,iset)
		) r.add (PortSet.empty,IntSet.empty)
  in
    IntMap.fold (fun _ lhs_i (pset,iset) -> 
		   Solution.AA.fold (fun id ag (pset,iset) ->
				       let inst_msg_list = 
					 try IntMap.find id r.actions with Not_found -> []
				       in
					 List.fold_right 
					   (fun (inst,_) (pset,iset) ->
					      match inst with
						  Solution.Bind (s,id',s') -> 
						    (PortSet.add (id',s'^"!") (PortSet.add (id,s^"!") pset),
						     IntSet.add id' (IntSet.add id iset))
						| Solution.Break (s,id',s') -> 
						    (PortSet.add (id',s'^"!") (PortSet.add (id,s^"!") pset),
						     IntSet.add id' (IntSet.add id iset))
						| Solution.Mark (s,mk) -> 
						    (PortSet.add (id,s^"~") pset,IntSet.add id iset)
						| Solution.Modify s -> (PortSet.add (id,s^"!") pset,IntSet.add id iset)
						| Solution.Remove -> 
						    Agent.fold_interface  
						      (fun s _ (pset,iset) ->
							 (PortSet.add (id,s^"!") (PortSet.add (id,s^"~") pset),
							  IntSet.add id iset) 
						      ) ag  (pset,iset)
					   ) inst_msg_list (pset,iset)
				    ) (Solution.agents lhs_i) (pset,iset)
		) r.lhs (pset,iset)
    
let static_cause r1 r2 =
  try
    let lense1,ids1 = mod_quarks r1 
    in
      try
	IntMap.fold (fun i lhs2_i b ->  (*fold r2.lhs*)
		       IntMap.fold 
			 (fun j rhs1_j b ->
			    let assoc_list = 
			      match Solution.pushout rhs1_j lhs2_i with
				  None -> []
				| Some assoc_list -> assoc_list
			    in
			      List.fold_right
				(fun assoc1_2 b -> 
				   IntSet.fold
				     (fun id1 b ->
					try
					  let id2 = IntMap.find id1 assoc1_2 in
					  let ag2 = Solution.agent_of_id id2 lhs2_i in
					  let quarks2 = 
					    Agent.fold_interface (fun site (inf,lnk) set ->
								    let set = 
								      match inf with
									  Agent.Wildcard -> set
									| _ -> PortSet.add (id1,site^"~") set
								    in
								    let set =
								      match lnk with
									  Agent.Wildcard -> set
									| _ -> PortSet.add (id1,site^"!") set
								    in
								      set
								 ) ag2 PortSet.empty
					  in
					  let inter = PortSet.inter lense1 quarks2 in
					    if PortSet.is_empty inter then b else raise True
					with Not_found -> b
				     ) ids1 b
				) assoc_list b
			 ) (Solution.split r1.rhs) b
		    ) r2.lhs false
      with True -> true
	| Not_found -> (Printf.printf "in Rule2.static_cause 1: "; raise Not_found)
  with
      exn -> (Printf.printf "Rule2.static_cause"; raise exn)

let (<<) = static_cause

let static_conflict r1 r2 =
  let lense1,ids1 = mod_quarks r1 
  in
    try
      IntMap.fold (fun i lhs2_i b ->  (*fold r2.lhs*)
		     IntMap.fold 
		       (fun j lhs1_j b ->
			  let assoc_list = 
			    match Solution.pushout lhs1_j lhs2_i with
				None -> []
			      | Some assoc_list -> assoc_list
			  in
			    List.fold_right
			      (fun assoc1_2 b -> 
				 PortSet.fold
				   (fun (id1,x1) b ->
				      try
					let id2 = IntMap.find id1 assoc1_2 in
					let ag2 = Solution.agent_of_id id2 lhs2_i in
					let quarks2 = 
					  Agent.fold_interface (fun site (inf,lnk) set ->
							    
							    let set = 
							      match inf with
								  Agent.Wildcard -> set
								| _ -> PortSet.add (id2,site^"~") set
							    in
							    let set =
							      match lnk with
								  Agent.Wildcard -> set
								| _ -> PortSet.add (id2,site^"!") set
							    in
							      set
							 ) ag2  PortSet.empty
					in
					  if PortSet.mem (id2,x1) quarks2 then raise True
					  else b
				      with Not_found -> b
				   ) lense1 b
			      ) assoc_list b
		       ) r1.lhs b
		  ) r2.lhs false
    with True -> true
      | Not_found -> (Printf.printf "in Rule2.static_cause 1: "; raise Not_found)

let (%>) = static_conflict 

 
type modif_type = Bound of (int*string) 
		  | Break of (int*string)
		  | Side_break of (int*string) (*Break a bond by side effect of a deletion or a rule of the form A(x!_)->A(x)*)
		  | Marked of (string*string) 
		  | Remove
		  | Init_mark of (string*string)     (*(agent_name * mark)*)
		  | Init_free of string              (* agent name*)
		  | Init_bound of (string*int*string) (*agent name, target id, target site *)
		  | Test_bound of (int*string) 
		  | Test_marked of string 
		  | Test_any_bound 
		  | Test_free
		  | Before_After of (modif_type*modif_type) (*hack to deal with story compression *)

(*modif label is a pure test, no modification of the quark occurs*)
let rec is_pure_test states =
  match states with
      [] -> true
    |  (Test_bound _)::tl | (Test_marked _)::tl | Test_any_bound::tl | Test_free::tl  -> is_pure_test tl
    | (Before_After (_,x))::tl -> (is_pure_test [x]) && (is_pure_test tl)
    | _ -> false (*the rest also contains modifs*)

(*modif label contains a test, and may also be a modification*)
let rec contains_test states = 
  match states with
      [] -> false
    | (Side_break _)::tl -> contains_test tl
    | (Before_After (_,x))::tl -> (contains_test [x]) or (contains_test tl)
    | _ -> true

(*modif label also contains a modification*)
let rec contains_modif states =
  match states with
      [] -> false
    | (Bound _)::tl | (Break _)::tl | (Side_break _)::tl | (Marked _)::tl | (Init_mark _)::tl 
    | (Init_free _)::tl | (Init_bound _)::tl -> true
    | (Before_After (_,x))::tl -> (contains_modif [x]) or (contains_modif tl)
    | Remove::_ -> true
    | _::tl -> contains_modif tl

let rec is_creation states = 
  match states with
      [] -> false
    | (Init_mark _)::_ | (Init_free _)::_ | (Init_bound _)::_ -> true
    | (Before_After (_,x))::tl -> (is_creation [x]) or (is_creation tl)
    | _::tl -> is_creation tl 

let rec is_deletion states =
  match states with
      [] -> false
    | Remove::tl -> true
    | (Before_After (_,x))::tl -> (is_deletion [x]) or (is_deletion tl)
    | _::tl -> is_deletion tl

let rec string_of_modif_type m =
  match m with
  | Before_After (_,x) -> string_of_modif_type x 
  | Bound (i,s) | Init_bound (_,i,s) -> Printf.sprintf "Bound(%d,%s)" i s
  | Break (i,s) -> Printf.sprintf "Break(%d,%s)" i s
  | Side_break (i,s) -> Printf.sprintf "Side Break(%d,%s)" i s
  | Marked (s,s') -> Printf.sprintf "Marked(%s->%s)" s s'
  | Init_mark (_,s) -> Printf.sprintf "Marked(%s)" s 
  | Remove -> "Remove" 
  | Test_bound (i,s) -> Printf.sprintf "Test bound(%d,%s)" i s
  | Test_marked s -> Printf.sprintf "Test marked %s" s
  | Test_any_bound -> "Test any bound"
  | Test_free -> "Test free"
  | Init_free _ -> "Free"
    
let apply r assoc sol = (*sol is imperative!*)
  try
    let lnk s = s^"!"
    and inf s = s^"~"
    in
    let mod_quarks,assoc_add,sol' = (*adding new agents in solution*)
      IntMap.fold (fun id ag (mod_quarks,assoc_add,sol') ->
		     let id_sol,fresh_id = 
		       try (IntMap.find id assoc_add,(Solution.fresh_id sol'))
		       with Not_found -> (Solution.fresh_id sol',(Solution.fresh_id sol'+1))
		     in
		     let sol' = Solution.add ~with_id:id_sol ag sol' (*with a given id already attributed*)
		     in
		       
		     let assoc_add = IntMap.add id id_sol assoc_add in 
		     let sol' = {sol' with Solution.fresh_id = fresh_id}
		     in
		     let mod_quarks = 
		       Agent.fold_interface (fun site (info,_) mq ->
					       let mq = 
						 match info with
						     Agent.Marked s -> 
						       PortMap.add (id_sol,inf site) [Init_mark (Agent.name ag,s)] mq
						   | _ -> mq
					       in
						 (*added agent is assumed to be free, bound are dealt with later on*)
						 PortMap.add (id_sol,lnk site) [Init_free (Agent.name ag)] mq
					    ) ag mod_quarks 
		     in
		       (mod_quarks,assoc_add,sol')
		  ) r.add (PortMap.empty,IntMap.empty,sol)
    in
    let phi id = 
      try IntMap.find id assoc 
      with Not_found -> 
	begin
	  try IntMap.find id assoc_add 
	  with Not_found -> runtime "Rule.apply: cannot find id in phi"
	end
    in
    let mod_quarks,rm_quarks,sol',warn =
      IntMap.fold (fun id act_msg_list (mod_quarks,rm_quarks,sol',warn) -> 
		     let id_sol = phi id in
		       List.fold_right 
			 (fun (act,_) (mod_quarks,rm_quarks,sol',warn) ->
			    match act with
				Solution.Bind (site,id',site') -> 
				  let id_sol'=phi id' in
				  let sol'= Solution.bind (id_sol,site) (id_sol',site') sol' in
				  let modif1 = 
				    if IntMap.mem id r.add then
				      let ag_new = IntMap.find id r.add in
					Init_bound (Agent.name ag_new,id_sol',lnk site')
				    else
				      Bound (id_sol',lnk site')
				  and modif2 = 
				    if IntMap.mem id' r.add then
				      let ag_new' = IntMap.find id' r.add in
					Init_bound (Agent.name ag_new',id_sol,lnk site)
				    else
				      Bound (id_sol,lnk site)
				  in
				  let mod_quarks = 
				    let l = try PortMap.find (id_sol,lnk site) mod_quarks with Not_found -> [] in
				      (PortMap.add (id_sol,lnk site) (modif1::l) mod_quarks)
				  in
				  let mod_quarks = 
				    let l = try PortMap.find (id_sol',lnk site') mod_quarks with Not_found -> [] in
				      (PortMap.add (id_sol',lnk site') (modif2::l) mod_quarks)
				  in
				    (mod_quarks,rm_quarks,sol',warn)
			      | Solution.Break (site,id',site') ->
				  let id_sol' = phi id' in
				  let sol' = Solution.unbind (id_sol,site) (id_sol',site') sol' in
				  let mod_quarks = 
				    PortMap.add (id_sol',lnk site') [Break (id_sol,lnk site)]
				      (PortMap.add (id_sol,lnk site) [Break (id_sol',lnk site')] mod_quarks)
				  in
				    (mod_quarks,rm_quarks,sol',warn)
				      
			      | Solution.Mark (site,mk) ->
				  let ag = Solution.agent_of_id id_sol sol' in
				  let sol' = Solution.mark (id_sol,site,mk) sol' in (*side effect on sol'*)
				  let (info,_) = Agent.state ag site in
				  let old_mk = 
				    match info with
					Agent.Wildcard -> "*"
				      | Agent.Marked s -> s
				      | _ -> runtime "Rule.apply: invalid info marker"
				  in
				  let mod_quarks = PortMap.add (id_sol,inf site) [Marked (old_mk,mk)] mod_quarks in
				    (mod_quarks,rm_quarks,sol',warn)

			      | Solution.Modify site ->
				  let (id_sol',site') = 
				    try Solution.get_port (id_sol,site) sol' 
				    with Not_found -> runtime "Rule.apply: assoc invariant violation in Modify case"
				  in
				  let sol' = Solution.unbind (id_sol,site) (id_sol',site') sol' in
				  let mod_quarks = 
				    PortMap.add (id_sol',lnk site') [Break (id_sol,lnk site)]
				      (PortMap.add (id_sol,lnk site) [Break (id_sol',lnk site')] mod_quarks)
				  in
				    (mod_quarks,rm_quarks,sol',true)
				      
			      | Solution.Remove -> 
				  let ag_sol = 
				    try Solution.agent_of_id id_sol sol' 
				    with Error.Runtime msg -> runtime ("Rule.apply:"^msg)
				  in
				  let mod_quarks,rm_quarks =
				    Agent.fold_interface  
				      (fun site _ (pmap,pset) ->
					 let pmap,pset =
					   try
					     let (i,x) = Solution.get_port (id_sol,site) sol' in
					       (*mod quark*)
					       (PortMap.add (i,lnk x) [Side_break (id_sol,lnk site)] pmap, 
						(*removed quark*)
						PortSet.add (id_sol,lnk site) pset)
					   with Not_found ->
					     (pmap,PortSet.add (id_sol,lnk site) pset)
					 in
					   (pmap,PortSet.add (id_sol,inf site) pset)
				      ) ag_sol  (mod_quarks,rm_quarks)
				  in
				  let warn',sol' = Solution.remove id_sol sol'
				  in
				    (mod_quarks,rm_quarks,sol',warn or warn')
				      
			 ) act_msg_list (mod_quarks,rm_quarks,sol',warn)
		  ) r.actions (mod_quarks,PortSet.empty,sol',false)
    in
      
    let tested_quarks = 
      IntMap.fold (fun _ lhs_i pmap -> 
		     AA.fold (fun id ag pmap ->
				let id_sol = phi id in
				  Agent.fold_interface 
				    (fun site (info,link) pmap -> 
				       let pmap = 
					 if PortMap.mem (id_sol,lnk site) mod_quarks then pmap
					 else
					   match link with
					       Agent.Bound -> (
						 try
						   let (id',site') = Solution.get_port (id,site) lhs_i in
						   let id_sol'= phi id'
						   in
						     PortMap.add (id_sol,lnk site) 
						       [Test_bound (id_sol',lnk site')] pmap
						 with 
						     Not_found -> 
						       PortMap.add (id_sol,lnk site) [Test_any_bound] pmap
					       )
					     | Agent.Free -> PortMap.add (id_sol,lnk site) [Test_free] pmap 
					     | _ -> pmap (*wildcard*)
				       in
					 if PortMap.mem (id_sol,inf site) mod_quarks then pmap
					 else
					   match info with
					       Agent.Marked mk -> PortMap.add (id_sol,inf site) [Test_marked mk] pmap
					     | _ -> pmap (*wildcard*)
				    ) ag pmap
			     ) lhs_i.Solution.agents pmap
		  ) r.lhs PortMap.empty
    in
      (mod_quarks,rm_quarks,tested_quarks,assoc_add,sol',warn)
  with exn -> (Printf.printf "In Rule.apply:" ; flush stdout; raise exn)

let print r = 
  let str = 
    match r.flag with
	None -> r.input
      | Some s -> s 
  in
    print_string str ; print_newline()

(*actions:(int * ((Solution.action*Solution.msg) list) ) list*)
exception Opposite of (modif_type list PortMap.t)
exception Not_opposite

let opposite modif_of_w modif_of_w' sol =
  let fuse_quark (i,s) = (*Not very elegant...*)
    let s = String.sub s 0 ((String.length s)-1) in (i,s)
  in
  let check_opp map1 map2 =
    PortMap.iter (fun w modif_list ->
		    if is_pure_test modif_list then
		      try
			let modif_list' = PortMap.find w map2 in
			  if List.hd modif_list = List.hd modif_list' then () 
			  else raise Not_opposite
		      with
			  Not_found -> 
			    let state = List.hd modif_list and (i_w,s_w) = fuse_quark w in
			      match state with
				| Test_bound w' -> (
				    let (i,s) = fuse_quark w' in
				      try
					let (i',s') = Solution.get_port (i_w,s_w) sol 
					in
					  if (i'=i) && (s'=s) then () else raise Not_opposite
				      with 
					  Not_found -> raise Not_opposite
				  )
				| Test_marked m -> 
				    if AA.mem i_w sol.Solution.agents then 
				      let ag = Solution.agent_of_id i_w sol in 
				      let (inf,_) = Agent.state ag s_w in
					match inf with
					    Agent.Marked m' -> if m=m' then () else raise Not_opposite
					  | _ -> raise Not_opposite
				    else raise Not_opposite
				| Test_any_bound -> 
				    if AA.mem i_w sol.Solution.agents then 
				      let ag = Solution.agent_of_id i_w sol in 
				      let (_,lnk) = Agent.state ag s_w in
					match lnk with
					    Agent.Bound -> () 
					  | _ -> raise Not_opposite
				    else raise Not_opposite
				| Test_free -> 
				    if AA.mem i_w sol.Solution.agents then 
				      let ag = Solution.agent_of_id i_w sol in 
				      let (_,lnk) = Agent.state ag s_w in
					match lnk with
					    Agent.Free -> () 
					  | _ -> raise Not_opposite
				    else raise Not_opposite
				| _ -> Error.runtime "Not a valid test"
		    else (*modif_list contains modification node*)
		      let opp_state state state' =
			match (state,state') with
			    (Marked (m1,m1'),Marked(m2',m2)) ->
			      if (m1=m2) && (m1'=m2') then () 
			      else raise Not_opposite
			  | (Bound (i,s),Break(i',s')) | (Break (i,s),Bound(i',s')) 
			  | (Bound (i,s), Side_break (i',s')) | (Side_break (i,s),Bound (i',s')) -> 
			      if (i=i') && (s=s') then () 
			      else raise Not_opposite
			  | _ -> raise Not_opposite
		      in
			try
			  let modif_list' = PortMap.find w map2 in
			    match modif_list with
				[state] ->
				  begin
				    match modif_list' with
					[state'] -> opp_state state state'
				      | _ -> raise Not_opposite (*multi action versus simple one*)
				  end
			      | [state1;state2] ->
				  begin
				    match modif_list' with
					[state1';state2'] -> (opp_state state1 state2' ; opp_state state2 state1') 
				      | _ -> raise Not_opposite (*multi action versus simple one*)
				  end
			      | _ -> Error.runtime "Rule.opposite: malformed modif_list"
			with Not_found -> raise Not_opposite 
		 ) map1
  in
    try
      let _ = check_opp modif_of_w modif_of_w' in
      let _ = check_opp modif_of_w' modif_of_w in
	raise (Opposite modif_of_w') (*raise an exception to interrupt search in network*)
    with Not_opposite -> () (*let search for an opposite proceed*)

(*((Solution.action * Solution.msg) list) IntMap.t *)
let str_of_actions r =
  let add = IntMap.fold (fun id ag cont -> 
			   (Printf.sprintf "ADD %s" (Agent.to_str ag))::cont
			) r.add []
  in
  String.concat "\\n"
    (IntMap.fold (fun id act_msg_list cont -> 
		    cont@(List.map (fun (act,msg) -> Solution.string_of_action id act) act_msg_list)
		 ) r.actions add)

let to_dot r = 
  if IntMap.is_empty r.lhs then 
    "digraph G{ label = \""^(str_of_actions r)^"\";"
    ^"subgraph cluster_empty{" 
    ^"style = filled; color = lightgrey ; label =\"\" a [label=\"\",shape=plaintext]}}"
  else
    Solution.to_dot2 (str_of_actions r) r.lhs 


let rec injections_product list_assoc_map = 
  match list_assoc_map with
      [] -> [IntSet.empty,IntMap.empty]
    | assoc_map::tl -> 
	let tl_assoc_map = injections_product tl in
	  List.fold_left (fun cont (ids,m) ->
			    AssocArray.fold (fun _ assoc cont ->
					       try 
						 let (ids',m') = 
						   IntMap.fold (fun id id' (ids,m) ->
								  if IntSet.mem id' ids then raise False
								  else (IntSet.add id' ids,
									IntMap.add id id' m)
							       ) assoc (ids,m)
						 in
						   (ids',m')::cont
					       with False -> cont
					    ) assoc_map cont
			 ) [] tl_assoc_map

(*let automorphism r = 
  let sol = Solution.fuse_cc r.lhs in 
  let list_assoc_map = 
    IntMap.fold (fun i cc_i list_assoc_map -> 
		   let precomp = IntMap.find i r.precompil in
		   let assoc_map_i = Solution.unify ~rooted:true (precomp,cc_i) (sol.Solution.agents,sol) in
		     assoc_map_i::list_assoc_map
		) r.lhs []
  in
  let m = Solution.injections_product list_assoc_map in IntMap.size m
*)

let contains_deletion r = 
  let rec find_rm act_msg_list =
    match act_msg_list with
	[] -> false
      | (Solution.Remove,_)::tl -> true
      | (Solution.Modify _,_)::tl -> true (*correction BUG semi-link 19/03/2008*)
      | _::tl -> find_rm tl
  in
    try 
      let b = IntMap.fold (fun _ act_msg_list b -> 
			     if b then b 
			     else
			       find_rm act_msg_list
			  ) r.actions false
      in
	if b then (raise True) else false
    with
	True -> true

let subs_act sigma modif =
  let f x = try IntMap.find x sigma with Not_found -> x in
  let rec apply f act = 
    match  act with   
	Bound (j,b) -> Bound (f j,b)
      | Test_bound (j,b) -> Test_bound (f j,b)
      | Side_break (j,b) -> Side_break (f j,b) 
      | Break (j,b) -> Break (f j,b)
      | Init_bound (x,j,y) -> Init_bound (x,f j,y)
      | Test_marked _ 
      | Test_any_bound  
      | Test_free 
      | Remove 
      | Marked _  
      | Init_mark _    
      | Init_free _ -> act
      | Before_After (x,y) -> Before_After (apply f x,apply f y) in 
    apply f modif 
      
let subs_rule sigma (rule,modif) = 
  let f x = try IntMap.find x sigma with Not_found -> x in
  let rule' = rule in
  let modif' = 
    PortMap.fold 
      (fun ((i,a)) act  -> 
	 PortMap.add ((f i,a)) (subs_act sigma  act))
      modif PortMap.empty 
  in rule',modif' 

