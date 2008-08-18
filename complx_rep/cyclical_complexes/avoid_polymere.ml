open Tools 
open Pb_sig 
open Kleenean_expr
open Abstract_expr_sig
open Error_handler 

module PortMap = Map2.Make (struct type t = string * string * string let compare = compare end)
module PortSet = Set.Make (struct type t = string*string*string let compare= compare end) 

let debug = false


let print_port (a,b,c) = 
  (print_string b;
  print_string c)

let succ x = 
  match x with None -> None 
  | Some x -> Some (x+1)

let string_of x = 
  match x with None -> ""
  | Some x -> ("."^(string_of_int x))

let smaller k k' = 
  match k' with 
    None -> true 
  | Some k' -> k<=k'


let error i = 
  unsafe_frozen None (Some "avoid_polyme.ml") (Some ("line  "^(string_of_int i))) None  (fun _ -> raise Exit)

type mode = Warn | Refine_with_constrains | Refine_without_constrains  

module Avoid_poly = 
  (functor (A:AbstractExprBool) -> 
    (struct 

      
let avoid_polymere file sub k kin_coef pb mode (l,m) = 
  let rule_warning = pb.rule_warning in 
  let cyclical = !Config_complx.only_closing_rules in   
  let cpb = 
    match pb.intermediate_encoding 
    with None -> error 6 
    | Some a -> a in
  let boolean_encoding = 
    match pb.boolean_encoding 
    with None -> error 7 
    | Some boolean_encoding -> boolean_encoding in
  let rule_system = 
    match sub with None -> boolean_encoding.system 
    | Some a -> a in 
  let contact_map = 
    match pb.contact_map 
    with None -> error 14
    | Some a -> a in
  let interface = cpb.cpb_interface in
  let interface = 
    List.fold_left
      (fun map (a,b,c) -> 
	let set = 
	  List.fold_left 
	    (fun set x -> StringSet.add x set)
	    StringSet.empty 
	    c in
	StringMap.add a set map)
      StringMap.empty 
      interface in 
  let interface x = 
    try 
      StringMap.find x interface 
    with
      Not_found -> StringSet.empty in 
  let extend_a_rule_class rule_class (l,m) = 
    let rule = 
      match rule_class.rules  with [t] -> t
      |	_ -> error 20 in 
    let control = rule_class.control  in 
    begin
      if not 
	  (List.exists 
	     (fun (b,bool) -> 
	       match b with L(_,_)  -> bool
	     | _ -> false)
	     control.context_update)
      then
	(if mode = Warn then [] else [rule_class]),(l,m) 
      else
	begin
	  let here = 
	    List.fold_left 
	      (fun set (b,bool) -> 
		match b,bool with 
		  Pb_sig.H(a,a'),bool -> StringSet.add a set
		| _ -> set)
	      StringSet.empty 
	      rule.injective_guard in  
	  let agent_map  = (* map agent name to a list of agent id *)
	    List.fold_left 
	      (fun map (id,ag) -> 
		if StringSet.mem id here then 
		  let old = 
		    try 
		      StringMap.find ag map 
		    with 
		      Not_found -> []
		  in
		  StringMap.add ag (id::old) map
		    else map)
	      StringMap.empty 
	      rule_class.id in
	  let agent_fresh = (*map each agent to its max id*) 
	    StringMap.map 
	      (fun l -> 
		match l with 
		  [] -> ""
		| t::q -> 
		    List.fold_left 
		      (fun a b -> if compare a b < 0 then b else a)
		      t l )
	      agent_map in
	  let new_id ag agmap agfresh = 
	    let old = 
	      try 
		StringMap.find ag agfresh  
	      with 
		Not_found -> "" in
	    let new_id = 
	      if old = "" then ag 
	      else (old^"'")
	    in
	    let oldlist = 
	      try 
		StringMap.find ag agmap 
	      with 
		Not_found -> [] 
	    in
	    StringMap.add ag (new_id::oldlist) agmap,
	    StringMap.add ag new_id agfresh,
	    new_id in 

	  let graph,graph2,cons  = 
	    List.fold_left 
	      (fun (portmap,graph2,cons) (b,bool) -> 
		match b,bool with 
		  B(a,b,s),false -> 
		    PortMap.add (a,b,s) None portmap,graph2,cons
		| L((a,b,s),(a',b',s')),true ->
		    PortMap.add (a,b,s) (Some (a',b',s')) 
		      (PortMap.add (a',b',s') (Some (a,b,s)) portmap),
		    (let fadd a b map = 
		      let old = 
			try
			  StringMap.find a map 
			with 
			  Not_found -> StringSet.empty 
		      in 
		      StringMap.add a (StringSet.add b old) map 
		    in
		    fadd a a'
		      (fadd a' a graph2)),cons
		| Dis((a,_),(c,_)),true  -> portmap,graph2,
		    String2Set.add (c,a) (String2Set.add (a,c) cons)
		| _ -> portmap,graph2,cons)
	      (PortMap.empty,StringMap.empty,String2Set.empty)
	      rule.injective_guard
	  in
	 
	  let rec connected reste sol = 
	    if StringSet.is_empty reste 
	    then 
	      sol
	    else
	      let min = StringSet.min_elt reste in
	      let rec visit wl bl = 
		match wl with [] -> bl
		| t::q ->
		    if StringSet.mem t bl then 
		      visit q bl 
		    else 
		      let bl = StringSet.add t bl in 
		      let neighboor = 
			try 
			  StringMap.find t graph2 
			with 
			Not_found -> StringSet.empty in 
		      let neighboor = StringSet.diff neighboor bl in 
		      visit 
			(StringSet.fold (fun a l -> a::l) neighboor wl)
			bl in
	      let comp = visit [min] StringSet.empty in
	      let reste = StringSet.diff reste comp in
	      let sol = 
		StringSet.fold
		  (fun a sol -> StringMap.add a comp sol)
		  comp sol in
	      connected reste sol in
	  let equiv = 
	    let m = connected here StringMap.empty in
	    let f x y = 
	      try 
		StringMap.find x m == StringMap.find y m 
	      with Not_found -> false 
	    in
	    f in

	  let new_bonds = 
	    List.fold_left 
	      (fun sol (b,bool) -> 
		match b,bool with 
		  L((x,a,b),(y,c,d)),true when not (equiv x y) -> 
		    if (String2Set.exists 
			  (fun (u,v) -> equiv x u && equiv y v)
			  cons)
			  
		    then sol else ((x,a,b),(y,c,d))::sol
		| _ -> sol)
	      []
	      control.context_update 
	  in
	 
	  if new_bonds = [] 
	  then 
	    	(if mode = Warn then [] else [rule_class]),(l,m) 
	  else 
	    let bmap = 
	    List.fold_left
	      (fun bmap (b,bool) -> 
		BMap.add b bool bmap)
	      BMap.empty 
	      rule.injective_guard
	    in 
	    let fadd port depth map = 
	      try 
		let old = 
		  PortMap.find port map 
		in 
		PortMap.add port (depth::old) map
	      with 
		Not_found -> 
		  PortMap.add port [depth] map in 
		    
	    let rec vide list_of_rules sol nref (l,m) boolflag  = 
	      match list_of_rules 
	      with 
		[] -> sol,(l,m)
	      | (rule_class,agmap,agfresh,graph,bmap,first_seen)::q' -> 
		  let _ = 
		    if debug then 
		      let _ = print_string "AGENTS" in
		      let _ = print_newline () in 
		      let _ = 
			StringMap.iter
			(fun a l -> 
			  print_string a;
			  List.iter print_string l;
			  print_newline ())
			  agmap in ()  
		  in 
		  let rule = 
		    match rule_class.rules  with [t] -> t
		    |	_ -> error 20 in 
		  let rec explore working_list black_list port_list unspecified_ports= 
		    match working_list 
		    with [] -> port_list,unspecified_ports 
		    | ((id,ag,site),depth,base,(agent',site'))::q when
			PortSet.mem (id,ag,site) black_list  -> 
			  explore q black_list 
			    port_list
			    unspecified_ports 
		    | ((id,ag,site),depth,base,(agent',site'))::q ->
			let black_list' = 
			  PortSet.add (id,ag,site) black_list in
			try 
			  let depth' = depth+1 in 
			  let site_set = 
			    StringSet.remove
			      site (interface ag) 
			  in
			  let wl,port_list,unspecified  = 
			    StringSet.fold 
			      (fun s' (q,p1,p2)  ->
				try 
				  match 
				    PortMap.find (id,ag,s') graph 
				  with 
				  None -> 
				    q,
				    fadd (id,ag,s') depth' p1,
				    p2 
				  |	Some (port) -> 
				      (port,depth',base,(ag,s'))::q,
                                      fadd port depth' p1,
				    p2
				with Not_found -> 
				  q,p1,fadd (id,ag,s') (depth',base) p2)
			      site_set
			      (q,
			       port_list,
			       unspecified_ports)
			  in  
			  explore 
			    wl
			    black_list'
			    port_list
			    unspecified 
			    
			with 
			  Not_found -> 
			    explore 
			      q 
			      black_list'
			      port_list 
			      (PortMap.add (id,ag,site) [depth,base] unspecified_ports)
		  in
		let port_list,unspecified_port = 
		  explore 
		    (List.fold_left
		       (fun list ((a,b,c),(a',b',c')) -> 
			 ((a,b,c),0,a,(b',c'))::((a',b',c'),0,a',(b,c))::list)
		       [] new_bonds)
		    PortSet.empty
		    PortMap.empty
		    PortMap.empty in 
		let potential_port = 
		  let rec aux liste sol = 
		    match liste with 
		      (path,init,(a,b,c),d,base,black)::q -> 
			if not (smaller d k) then aux q sol 
			else
			  begin
			  let list = 
			    try 
			      String2Map.find (b,c) 
				contact_map.link_of_site  
			    with 
			      Not_found -> []
			  in
			  let sol'= 
			    List.fold_left
			      (fun a b -> 
				((b,"")::path,init,b,d+1,base)::a)
			      sol
			      list  
			  in
			  let liste' = 
			    List.fold_left
			      (fun a (ag,ag',s) -> 
			     	let interface = 
				  StringSet.remove s (interface ag')
				in
				StringSet.fold
				  (fun a b -> 
				    if PortSet.mem (ag,ag',a) black then 
				      b
				    else
				      (((ag,ag',s),a)::path,init,(ag,ag',a),d+1,base,PortSet.add (ag,ag',a) black)::b)
				  interface 
				  a)
			      q 
			      list 
			  in
			  aux liste' sol'  
			  end
		    | [] -> sol in
		  aux (
		  PortMap.fold 
		    (fun (a,b,c) l1  l ->  
		      List.fold_left 
			(fun sol (d,e) -> 
			  ([],(a,b,c),(a,b,c),d,e,PortSet.empty)::sol)
			l l1 )
		    unspecified_port []) []  in
		
		 let flag = 
		      List.fold_left 
			(fun sol x  -> 
			  (match x.r_simplx.Rule.flag 
			    with None -> x.r_id 
			    | Some a -> a))
			"" rule.labels in 
		 let pp2 = 
		   (List.fold_left
		      (fun list ((a,b,c),(a',b',c')) -> 
			let i = interface b in
			let i = StringSet.remove c i in
			let list = 
			  StringSet.fold 
			    (fun s list -> 
			      ([],(a,b,s),(a,b,c),0,b)::list)
			    i list
			in	
			let list = 
			  StringSet.fold 
			    (fun s list -> 
			      ([],(a',b',s),(a',b',c'),0,b')::list)
			    i list 
			in
			list)
		      [] new_bonds) in
		 let potential_port = pp2@potential_port in
		 let _ = 
		    if debug  then 
		    let _ = print_string "RULES \n" in 
		    let _ = print_string "R" in 
		    let _ = print_string flag in 
		    let _ = print_newline () in 
		    let _ = 
		      BMap.iter 
			(fun b bool -> 
			  print_b b ;
			  print_string (if bool then "T" else "F");
			  print_newline ())
			bmap in 
		    let _ = print_string "PORT LIST \n" in
		    let _ = 
		      PortMap.iter 
			(fun (a,b,c) (k) -> 
			  print_string a;
			  print_string ".";
			  print_string c;
			  print_string "DEPTH: ";
			  List.iter print_int k ;
			  print_newline ())
			port_list in 
		    
		    let _ = print_string "UNSPECIFIED PORTS \n" in
		    
		    let _ = 
		      PortMap.iter 
			(fun (a,b,c) k -> 
			  print_string a;
			  print_string ".";
			  print_string c;
			  List.iter (fun (k,base) -> 
			    print_string "DEPTH: ";
			    print_int k;
			    print_string "BASE: ";
			    print_string base;
			    print_newline ()) k ;
			  print_newline ())
			unspecified_port in  
		      let _ = print_string "POTENTIAL PORTS \n" in
		      let _ = 
			List.iter 
			  (fun (a,init,(_,b,s),c,base) -> 
			    print_string b;
			    print_string ".";
			    print_string s;
			    print_string "DEPTH: ";
			    print_int c;
			    print_string "INIT: ";
			    let _ = 
			      let (a,b,c) = init in
			      (print_string a;
			      print_string b;
			      print_string c) 
			    in 
			    print_string "BASE: ";
			    print_string base;
			    print_string "PATH: ";
			    List.iter (fun ((a,b,c),d) -> print_string a;print_string b;print_string c;print_string d;print_string ",") a;
			    print_newline ())
			  potential_port in ()
	 	in 
		let pp = 
		  List.fold_left 
		    (fun map (a,init,(x,b,s),c,base) -> 
		      let old = 
			try 
			  StringMap.find b map 
			with 
			  Not_found -> [] in
		      StringMap.add b ((init,s,a,c,base,(x,b,s))::old) map)
		    StringMap.empty 
		    (potential_port) in
		let bool = 
		  StringMap.fold
		    (fun a b bool -> 
		      match bool with Some a -> Some a
		      |	None -> 
			  let rec aux liste = 
			    match liste with [] -> None
			    | (init,s,a,c,d,e)::q ->
				let rec aux2 liste2 = 
				  match liste2 with (init',s',a',c',d',e')::q2 ->  
				    if 
				      c+c'>0 
					&& 
				      smaller (c+c') k
					&&
				      d<>d' 
					&& 
			  	      s<>s'
				    then 
				      Some ((init,s,a,c,d,e),(init',s',a',c',d',e'))
				    else
				      aux2 q2
				  |  _ -> aux q in
				aux2 q
			  in aux b)
		    pp None
		in 
		let good_cycle l = 
		  let rec aux l black = 
		    match l with 
		      [] -> true
		    | ((b,_,c),a)::q -> 
			try 
			  match 
			    StringMap.find b black
			  with 
			  None -> false
			  | Some a' when a<>a' -> false
			  | _ -> aux q (StringMap.add b None black)
			with 
			  Not_found -> 
			    aux q (StringMap.add b (Some c) black)
		  in aux l StringMap.empty in 
(*		let print_half_path (init,s,a,c,d,e) = 
		  print_string "PORT";
		  print_port init;
		  print_string "\n s";
		  print_string s;
		  print_string "\n PATH";
		  List.iter 
		    (fun ((a,b,c),d) -> 
		      print_string d;
		      print_string b;
		      print_string c;
		      print_string ",")
		    a;
		  print_string "\n lENGTH:";
		  print_int c;
		  print_string "\n D";
		  print_string d;
		  print_port e in *)
		let cycles = 
		  if not first_seen then []
		  else
		     StringMap.fold
		      (fun a b list -> 
		   	let rec aux liste rep = 
			  match liste with [] -> rep  
			  | (init,s,a,c,d,e)::q ->
			      let rec aux2 liste2 rep = 
				match liste2 with (init',s',a',c',d',e')::q2 ->  
				  if 
				    a'= [] 
				      &&
				    c+c'>0 
				      && 
				    smaller (c+c') k
					&&
				    compare d d' < 0  
				      && 
			  	    s<>s'
				      && good_cycle a 
				  then 

				    (
				    
				     aux2 q2 
				      (match rep with (a',_)::_ when a'=(a,init) -> rep
				     | _ ->
					 (
					 (((a,init),init')::rep)))
				      )

				    else
				      aux2 q2  rep 
				|  _ -> aux q  rep in
				aux2 q  rep 
			  in aux b  list)
		    pp [] in 
		let sol,nref = (*closing cycles*)
		  if mode = Warn 
		  then sol,nref 
		  else 
		    List.fold_left 
		      (fun (sol,nref)  ((a,b),c) ->
			let inj = 
			  let rec aux rule (a0,id0,s0) list agmap agfresh = 
			    match list with [] -> rule
			    | ((a,id,s),s')::q -> 
				let rule' = rule in
				let agmap,agfresh,a'' = if s'="" then agmap,agfresh,let x,_,_ = c in x else new_id id agmap agfresh in 
				let site = (a0,id0,s0) in 
				let site' = (a'',id,s) in 
				let rule' = 
				  if s<> "" then 
				    (
				    (H(a'',id),true)::
				    (Pb_sig.l(site,site'),true)::
				    (B(site'),true)::
				    (B(site),true)::rule) else rule  in 
				
				aux rule' (a'',id,s')  q agmap agfresh
			  in aux rule.injective_guard b (List.rev a) agmap agfresh in
			let rule = 
			  {rule with injective_guard = inj ;
			    labels = 
			    List.map 
			      (fun x -> 
				{x with Pb_sig.r_id = flag^(string_of nref);
				  Pb_sig.r_simplx = 
				  {x.Pb_sig.r_simplx with 
				    Rule.kinetics = x.Pb_sig.r_simplx.Rule.kinetics *. kin_coef}
				}	
				  )
			      rule.labels 
			  } 
			in 
			{rule_class 
			with rules = [rule]}::sol,succ nref)
		      (sol,nref) cycles 
		in 
		let sol,nref = (* constraints *) 
		  if 
		    cycles = [] 
		      or mode = Warn 
		      or (not cyclical) then sol,nref else 
		  let cons = 
		    List.fold_left
		      (fun list ((a,x,_),(b,y,_)) -> 
			(Dis((a,x),(b,y)),true)::list)
		      rule.injective_guard 
		      new_bonds in 
		  let rule = 
		    {rule 
		    with injective_guard = cons ;
		      labels = 
		      List.map 
			(fun x -> {x with Pb_sig.r_id = flag^(string_of nref)})
			rule.labels 
		    } 
		  in 
		  {rule_class 
		  with rules = [rule]}::sol,succ nref 
		in 
		match bool  				  
		with None -> 
		  (vide 
		     q' 
		     (if mode=Warn then sol else rule_class::sol) 
		     nref 
		     (l,m) 
		     false)
		|   Some ((init,s,a,c,d,e),(init',s',a',c',d',e')) -> 
		    let free site nref base  =
			List.fold_left 
			  (fun (q,nref) (rule_class,agmap,agfresh,graph,bmap,first_seen) ->  
	                    let rule = 
			      match rule_class.rules with [rule] -> rule
				 | _ -> error 441 in  
			    let q,nref  = if cyclical then q,nref else 
			      if 
				(try let _ = PortMap.find site graph  in false  
				with _ -> true) 
				  &&
				(try not (BMap.find (B(site)) bmap)
				with _ -> true)
				  
			      then 
			      
				let rule = 
				  {rule 
				  with injective_guard = 
				    (B(site),false)::rule.injective_guard
						       ;
				    labels = 
				    List.map 
				      (fun x -> {x with Pb_sig.r_id = flag^(string_of nref)})
				      rule.labels 
				  } 
				in 
				({rule_class 
				 with rules = [rule]},agmap,agfresh,
				 PortMap.add site None graph,
				 BMap.add (B(site)) false bmap,false
			           )::q,succ nref  
			      else 
				q,nref 
			    in
			    let q,nref  = 
		      	      if 
				(try let _ = PortMap.find site graph 
in false  
				with _ -> true) 
				  &&
				(try (*not*) (BMap.find (B(site)) bmap) 
				with _ -> true)
			      then 
				begin(*1*)
				  let (a,b,c) = site in 
				  let list = 
				    try 
				      String2Map.find (b,c) 
					contact_map.link_of_site  
				    with 
				      Not_found -> []
				  in
				  List.fold_left 
				    (fun (q,nref) (_,b',s') ->
				      let l0 = 
					try StringMap.find b' agmap 
					with Not_found -> [] in 
				      let l = 
					List.filter 
					  (fun a'' -> 
					    (try 
		       			      (let _ = PortMap.find (a'',b',s') graph in false)  with Not_found -> true)
					      &&
					    ((*not*) (try BMap.find (B(a'',b',s')) bmap
					    with Not_found -> true)))
					  l0 in 
				      
				      begin(*2*)
					if l0 = [] 
					then 
					  begin(*3*)
					    let agmap,agfresh,a'' = new_id b' agmap agfresh in 
					    let site' = (a'',b',s') in 
					    let rule = 
					      {rule 
					      with injective_guard = 
					      (
						(H(a'',b'),true)::
						(Pb_sig.l(site,site'),true)::
						(B(site'),true)::
						(B(site),true)::rule.injective_guard)
						;
						labels = 
						List.map 
						  (fun x -> {x with Pb_sig.r_id = flag^(string_of nref)})
						  rule.labels 
					    } 
					    in
					  (*let agentset = a''::l0 in*) 
					    ({rule_class 
					   with rules = [rule]},
					     agmap,
					     agfresh,
					     PortMap.add 
					       site'
					     (Some site) 
					       (PortMap.add site (Some site') graph),
					     BMap.add (B(site')) true 
					       (BMap.add (Pb_sig.l(site,site')) true 
						  (BMap.add (B(site)) true bmap)),false )::q,succ nref
					  end(*3*) 
					else
					  (q,nref)
				      end(*2*)
			      )
				    
				    (q,nref) list 
				end(*1*) 
			      else
				(q,nref) in (q,nref)
			      ) ([],nref) base in
(*		    let free2 site site' nref base  =
			List.fold_left 
			  (fun (q,nref) (rule_class,agmap,agfresh,graph,bmap) ->  
	                    let rule = 
			      match rule_class.rules with [rule] -> rule
				 | _ -> error 441 in  
		
			    let q,nref  = 
		      	      if 
				(try let _ = PortMap.find site graph in false
				with _ -> true) 
				  &&
				(try (*not*) (BMap.find (B(site)) bmap) 
				with _ -> true)
				  &&
				(try let _ = PortMap.find site' graph in false
				with _ -> true) 
				  &&
				(try (*not*) (BMap.find (B(site')) bmap) 
				with _ -> true)
			      then 
				let (a,b,c) = site in 
				let (a',b',c') = site' in
				let list = 
				  try 
				    String2Map.find (b,c) 
				      contact_map.link_of_site  
				  with 
				    Not_found -> []
				in
				if List.exists (fun x -> x=(b',b',c')) list 
				then 
				  let rule = 
				    {rule 
				    with injective_guard = 
				      (
				      (Pb_sig.l(site,site'),true)::
				      (B(site'),true)::
				      (B(site),true)::rule.injective_guard)
					;
				      labels = 
				      List.map 
					(fun x -> {x with Pb_sig.r_id = flag^(string_of nref)})
					rule.labels 
				    } 
				  in
				  ({rule_class 
				   with rules = [rule]},
				   agmap,
				   agfresh,
				   PortMap.add 
				     site'
				     (Some site) 
				     (PortMap.add site (Some site') graph),
				   BMap.add (B(site')) true 
				     (BMap.add (Pb_sig.l(site,site')) true 
					(BMap.add (B(site)) true bmap)),false
				     )::q,succ nref  
				else
				  q,nref
			      else
				q,nref in q,nref )
			([],nref) base in *) 
		    let q,nref  = 
		      if mode = Warn 
		      then [],nref 
		      else
			let q,nref = free init nref 
			    [rule_class, 
			      agmap,agfresh,
			      graph,
			      bmap,first_seen] in 
			let q,nref = 
			  if q = [] then 
			    free init' nref [rule_class,agmap,agfresh,graph,bmap,false]
			  else
			    q,nref  in
			q,nref  in 
		    let m = 
		      if boolflag  then 
			("Rule "^flag^" might create polymers ")::m
		      else  m 
		    in 
		    vide (q@q') sol nref (l,m) false 
	  in 
	  vide [rule_class,agent_map,agent_fresh,graph,bmap,true] [] (Some 1) (l,m)  true  
	end
    end
  in 
  let new_system,(l,m)= 
    List.fold_left 
      (fun (system,(l,m)) a -> 
	let (r,(l,m)) = extend_a_rule_class a (l,m) in
	r@system,(l,m))
      ([],(l,m)) 
      (List.rev rule_system) in 
  {pb with refined_system = 
    IntMap.add 
      (match k with None -> (-1) 
      | Some k -> k) 
      {boolean_encoding with system =  new_system} pb.refined_system ;
    rule_warning = rule_warning },new_system,(l,m)
      
let dump_rs chan rs =
  let print_string = 
    match chan with None -> print_string 
    | Some a -> (fun x -> Printf.fprintf a "%s" x) in
  let print_newline  = 
    match chan with None -> print_newline 
    | Some a -> (fun x -> Printf.fprintf a "\n") in 
  let rs = 
    List.rev_map 
      (fun (a,a',b) -> 
	(a,
	 a',
	 List.rev_map 
	   (fun (a,b) -> A.K.E.V.var_of_b a,b) 
	   b)
	  ) 
      rs in
  let vars = 
    List.fold_left 
      (fun sol (_,_,b) -> 
	List.fold_left 
	  (fun sol (a,_) -> A.K.E.V.varset_add a sol)
	  sol b)
      A.K.E.V.varset_empty rs in
  let s = A.K.build_kleenean_rule_system rs vars in 
  let s = 
    A.K.print_kleenean_system (fun x->true) 
      (fun x -> 
	(match x.r_simplx.Rule.flag 
	with None -> x.r_id 
	| Some a -> a))
      (fun x -> 3) (IntSet.empty) 
      s (Some "()") (fun x->x) (fun x->x) true None in 

  let _ = 
    List.iter 
      (fun (a,b) -> 
	let flag,kynetic = 
	  match a with 
	    [r] -> let rid = r.Pb_sig.r_id in 
	           let old = name_of_rule r in 
		   (if string_prefix old rid then rid else old),
		   kynetic_of_rule r 
	  | _ -> "",1. in 
	let _ = print_string "'" in
	let _ = print_string flag in
	let _ = print_string "' " in
	let _ = List.iter print_string (List.rev b) in
	let _ = print_string " @ " in
	let _ = print_string (string_of_float kynetic) in
	let _ = print_newline () in 
	()) s
  in 
  () 

      
      let dump file pb  k = 
	let chan = 
	  if file = "" then None
	  else Some (open_out file) 
	in
	let _ = 
	  try 
	    let rs = IntMap.find k pb.refined_system in
	    let system = rs.system in
	    List.iter 
	      (fun rs -> 
		let inj = rs.rules in 
		List.iter 
		  (fun inj -> 
		    dump_rs chan  
		      [(inj.labels,rs.control,inj.injective_guard)])
		  inj)
	      (List.rev system)
	  with 
	    Not_found -> () 
	in
	let _ = match chan with None -> () | Some a -> close_out a in 
  () 
end))
