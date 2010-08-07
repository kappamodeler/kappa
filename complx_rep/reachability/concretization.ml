(* 13/04/2007 *)
(* Static analysis of Kappa systems*)
(* Jerome Feret for openKappa *)
(* Complexes enmeration *)
(* concretization.ml *)
(* Last mod: 2010/08/07*)

open Tools
open Data_structures 
open Pb_sig
open Config_complx
open Error_handler 



let print_ts ts = 
  (print_string ts.specie_name;
  print_string " ";
  print_string ts.site_name;
  print_string " ")

let ts_of_gs_site gs s = 
   {specie_name= gs.name;
     site_name= s}

let ts_of_int_site k s specie_map = 
  let gs = IntMap.find k specie_map  in 
  ts_of_gs_site gs s 
  

let empty_specie_map = 
  {number = 0;                      
    g_species = IntMap.empty;       
    name_of_gsid = (fun x -> "");   
    good_binding = (fun x y -> false); 
    which_binding  = (fun x -> []);    
    specielist_of_name  = (fun x -> []); 
    weigth_of_id  = (fun x -> 0);         
      get_sites  = (fun x -> [])}        


let print_complexe complexe spec log = 
  let pretty_map,name_of_path,map3 = 
    List.fold_right 
      (fun ip (map,map2,map3) -> 
	(StringListMap.add ip.path ((IntMap.find ip.isid spec.g_species).phospho_site) map),
	(StringListMap.add ip.path (IntMap.find ip.isid spec.g_species).name map2),
	(StringListMap.add ip.path ip.isid map3))
      complexe.agents
      (StringListMap.empty,StringListMap.empty,StringListMap.empty) in 
  let pretty_map_with_links,k= 
    let fadd p l n map = 
      let old = StringListMap.find p map in 
      let tuple = StringMap.find l old in 
      let new' = StringMap.add l {tuple with link = Init (bound_of_number n)} old in
      StringListMap.add p new' map in 
    let a,k =  ( List.fold_right (fun ((i,p,l),(i',p',l')) (map,n)-> 
      fadd p l n (fadd p' l' n map),n+1)
		  complexe.links (pretty_map,1)) 
    in a,k in 
  let _,l,_ = list_fold
      (fun a (bool,complexe,k) -> 
	let p = a.path in 
	let b = StringListMap.find p pretty_map_with_links in 
	let n = (StringListMap.find p name_of_path) in 
	let pretty = StringMap.add n b StringMap.empty in 
	let a,token,c = 
	  print_pretty 
	    string_txt 
	    n 
	    (fun _ -> true) 
	    ((pretty,pretty),k)
	    tuple_known 
	    (Some "()") 
	    (if bool then (!Config_complx.solution_separator) else "") 
	    (fun x -> x) 
	    (fun x->x) 
	    None 
	    log 
	in 
	a,((List.rev token)::complexe),c)
      complexe.agents (false,[],k) in 
  List.rev l,complexe.card


	
let print_spec spec log = 
  match log with None -> ()
  | Some log -> 
  IntMap.iter (fun i c ->
		 let pretty = StringMap.add c.name (c.phospho_site) StringMap.empty in 
		 let a,_,b = 
		   print_pretty 
		     string_txt 
		     c.name 
		     (fun _ -> true) 
		     ((pretty,pretty),1)
		     tuple_known 
		     (Some "()") 
		     "" 
		     (fun x-> x) 
		     (fun x -> x) 
		     None 
		     (Some log) in 
    if a 
    then 
      (Printf.fprintf log "\n")
    else ()
      ) spec.g_species
   
let print_list spec sol  = 
  let _ = print_complexe sol spec (Some stdout) in 
  List.iter 
    (fun a -> 
      List.iter print_string a.path;
      print_newline ())
    sol.agents


let test sol spec = 
  let rec aux liste root map map2 = 
    match liste with 
      [] -> root,map,map2
    | t::q -> aux q 
	  (if t.path = [] then Some t.isid else root) 
	  (StringListMap.add t.path t.isid map) 
	  (IntMap.add 
	     t.isid (let l = try (IntMap.find t.isid map2) with Not_found -> []
	     in (t.path)::l) map2)
  in 
  let r,map1,map2 = aux sol.agents None StringListMap.empty IntMap.empty in 
  let map3 = 
    List.fold_right 
      (fun ((i,p,s),(i',p',s')) sol -> 
	StringListMap.add (s::p) s' (StringListMap.add (s'::p') s sol))
      sol.links StringListMap.empty in 
  let r = match r with Some r -> r | None -> (print_string "EXIT0";raise Exit) in 
  List.for_all 
    (fun path -> 
      if path = [] 
      then true 
      else 
	let add path succ = 
	  match path with 
	    | [] -> [succ]
	    | t::q  -> 
		if succ = StringListMap.find path map3 
		then q 
		else succ::path
	in 
	let rec check p p' old = 
	  let ag = try (StringListMap.find p map1) with Not_found -> (raise Exit) in 
	  begin 
	    let ag' = StringListMap.find p' map1 in 
	    if ag =  ag' 
	    then 
	      let rec aux l = 
		match l with [] -> 0
		| a::q -> 
		    begin
		      let a' = StringListMap.find (a::p) map3 in 
		      let r = check (a::p) (add p' a) (Some a') in  
		      if r=0 
		      then 
			aux q 
		      else 
			r
		    end
	      in aux 
		(let l_site = (try (IntMap.find ag spec.g_species) with Not_found -> raise Exit).linkable_site  in  
		match old with None -> l_site
		|  Some forbid -> 
		    list_fold 
		      (fun a sol -> if a = forbid then sol else a::sol)
		      (List.rev l_site)
		      [])
	    else compare ag ag'
	  end
	in 
	check [] path None <=0 )
    (IntMap.find r map2) 
    
    
    
    
    
      
    
let complexe_of_gspec gspec  = 
  {card=gspec.weigth;
    agents = [{path=[];isid=gspec.gsid}];
    available_sites= List.rev_map (fun s -> gspec.gsid,([],IntMap.add gspec.gsid 1 IntMap.empty,
TSMap.empty,
IntMap.empty),s) gspec.linkable_site;
    links = [];open_links = []}

let get_gspecies prelist pb  messages =
  let error i x t y = 
    unsafe 
      (Some x) 
      (Some "Complx")
      (Some "concretization.ml") 
      (Some t) 
      (Some i) 
      y in 
  let cpb = 
    match pb.intermediate_encoding with 
      None -> error "line 177" "Intermediate encoding is missing" "get_gspecies" (raise Exit)
    | Some a -> a in 
  let contact = 
    match pb.contact_map with 
      None -> error "line 179" "Contact map is missing" "get_gspecies" (raise Exit)
    | Some a -> a in 
  let _ = trace_print "GET_species" in 
  let nmarks = List.length cpb.cpb_marks in 
  let image_of potential_binding = 
    let fadd a b map = 
      let l = try String2Map.find a map with Not_found -> [] in 
      String2Map.add a (b::l) map in 
    list_fold (fun (a,b) map -> 
      if a=b then fadd a b map
      else fadd a b (fadd b a map))
      potential_binding String2Map.empty in 
  let _ = trace_print  "IMAGE_OF" in 
  let image_of a  = 
    try (String2Map.find a (image_of contact.relation_list))
    with Not_found -> [] in 
  let gspec_of_conc x image_of = 
    let _ = trace_print "GSPECOFCONC" in 
    	list_fold (fun x sol -> 
    		let _ = 	trace_print "L1" in 
			list_fold 
	(fun (name,(b,pretty),h) sol -> 
	  (StringMap.fold 
	     (fun s tuple (weigth,name,current) -> 
	       let weigth'= weigth*(let f=match tuple.is_marked,tuple.mark
	                            with Not_initialized,_ | Init false,_ | _,Init _ -> (fun  g -> 1)
				    |  _  -> (fun  g -> (g ())) in
                                    
				    f (fun () -> 
				    	max 1 (nmarks - 
						 (match tuple.impossible_marks with 
							Init l -> List.length l 
							| _ -> 0)))) 
	       in 
	       let current' = 
		 if tuple.is_bound = Init false or tuple.is_bound = Not_initialized then (current)
		 else 
		   list_fold 
		     (fun (rho,available_sites) liste -> 
		       let bbound =
			 fst( 
			 list_fold 
			   (fun (t,t') (liste,f) ->  
			     if (match tuple.impossible_links  with Init d -> List.mem (t,t') d |  _ -> false) or (match tuple.link with Init (a,a') -> a<>t or a'<>t' | _ -> false) 
			     then liste,f
			     else (((StringMap.add s ({{{tuple 
							with is_bound = Init true}
						       with link = (Init (t,t'))}
						      with impossible_links = Init []})
				       rho,
				     ((s,(t,t'))::available_sites)))::liste,(t,t')::f))
			   (image_of (name,s)) (liste,[])) in
		       if tuple.is_bound  = Any or tuple.is_bound = Abstracted or tuple.is_bound =Not_initialized then 
			 ((StringMap.add s {tuple with is_bound = Init false}
			     rho,
			   available_sites)::bbound)
		       else bbound )
		     current [] in 
	       (weigth',name,current'))
	     pretty (1,name,[pretty,[]])
	  )::sol)
	x sol) x []
  in 
  let _ = trace_print "ESSAI" in 
  let l = gspec_of_conc prelist image_of in 
  let _ = trace_print "ENDESSAI" in 
  let m = list_fold (fun (a,b,c) k -> k+(List.length (c))) l 0 in 
  let tt = Array.make (m+1) "" in 
  let tu = Array.make (m+1) 0 in 
  let tv = Array.make (m+1) [] in 
  let tw = Array.make (m+1) StringMap.empty in 
  let rep,n,spec = 
    list_fold (fun (w,n,c) (map,k,spec) -> 
      list_fold (fun c (map,k,spec) -> 
	(tt.(k)<-n;
	 tu.(k)<-w;
	 tv.(k)<-List.rev_map fst (snd c);
	 tw.(k)<-list_fold (fun (s,a) map -> StringMap.add s a map) (snd c) StringMap.empty;
	 IntMap.add k {gsid = k;
			name =  n;
			linkable_site= tv.(k);
			phospho_site=fst c;
			weigth=w} map,k+1,
	 StringMap.add n 
	   (let l = try (StringMap.find n spec) with Not_found -> [] in 
	   k::l) spec)) 
	c (map,k,spec)
	)
      l 
      (IntMap.empty,1,StringMap.empty) in 
  {number = m;
    g_species=rep;
    name_of_gsid=(fun k -> tt.(k));
    weigth_of_id=(fun k -> tu.(k));
    get_sites = (fun k -> tv.(k));
    good_binding =  
    (fun (n,s) (n',s') -> 
      let g = tt.(n) in 
      let g'= tt.(n') in 
      StringMap.find s (tw.(n)) = (g',s') && 
      StringMap.find s' (tw.(n')) = (g,s));
    which_binding = 
    (let tz=Array.make (m+1) StringMap.empty in
    (fun (n,s) -> 
      let g = tt.(n) in 
      let g',s' = StringMap.find s (tw.(n)) in 
      try (StringMap.find s (tz.(n))) 
      with Not_found -> 
	(let rep =  
	  (let rec aux n' sol = 
	    if n' <=0 then sol
	    else 
	      aux (n'-1) 
		(if (try (StringMap.find s' (tw.(n')) = (g,s)) with _ -> false)
		then ((n',s')::sol)
		    
		else 
		  sol)
	  in aux m []) in 
	let _ = StringMap.add s rep (tz.(n)) in rep)));
    specielist_of_name=(fun s -> StringMap.find s spec)},messages     
    
let sort_complexe complexes spec = 
  let c2 = 
    List.rev_map (fun a -> 
      {a with agents = 
	List.sort (fun a b -> 
	  let ga = (IntMap.find a.isid spec.g_species).name in 
	  let gb = (IntMap.find b.isid spec.g_species).name in 
	  let r0 = compare ga gb in 
	  if r0 = 0 
	  then 
	    let r1 = compare a.isid b.isid in 
	    if r1 = 0 then compare a.path b.path else r1
	  else r0)
	  a.agents}) complexes in 
  List.sort (fun a b -> 
    let rec p1 l1 l2 =
      match l1,l2 with 
        [],[] -> 0 
      | t1::q1,t2::q2 -> 
	  let ga = (IntMap.find t1.isid spec.g_species).name in 
	  let gb = (IntMap.find t2.isid spec.g_species).name in 	
	  let r=compare ga gb in 
	  if r = 0 then p1 q1 q2 else r  
      | [],_ -> -1
      | _,[] -> 1 in
    let rec p2 l1 l2 = 
      match l1,l2 with 
	[],[] -> 0
      | t1::q1,t2::q2 -> 
	  let r = compare t1.isid t2.isid in 
	  if r = 0 then p2 q1 q2 else r
      | [],_ -> -1
      | _,[] -> 1 in
    let r0 = p1 a.agents b.agents in
    if r0=0 then p2 a.agents b.agents else r0)
    c2
    
                            
let log n = 
  let k = !Config_complx.log_each in 
  if k = 0 then ()
  else 
    if n mod k = 0 
    then 
      (print_int n;
       print_string " complexes";
       print_newline ())

let store s l = 
  if !Config_complx.dump_all_complexs then s::l 
  else l 

let concretization conc  messages = 
  let close complexe sol n = 
    match complexe.available_sites with 
      [] -> sol
    | (i,(p,m,m2,a),l)::av_sites ->
	let rec aux liste (av,sol) = 
	  match liste with [] -> sol
	  |	(i',(p',m',m2',a'),l')::q -> 
	      if conc.good_binding (i,l) (i',l')
	      then 
		aux q (((i',(p',m',m2',a'),l')::av),{{complexe with available_sites = av@q} with open_links =(i,p,l)::(i',p',l')::complexe.open_links}::sol)
	      else
		aux q (((i',(p',m',m2',a'),l')::av),sol )
	in aux av_sites ([],sol) 
  in 
  
  let enlarge complexe sol n ncomp =
    match complexe.available_sites with 
      [] -> sol
    | (i,(p,m,m2,a),l)::av_sites -> 
	let ts = ts_of_int_site i l (conc.g_species)  in 
	let k2 = try (TSMap.find ts m2) with Not_found -> 0 in 
	if k2>=1 then 
	    {card = complexe.card;
		    agents = complexe.agents;
		    available_sites = [];	     
		    links = complexe.links;
		    open_links = 
		    List.fold_left 
		      (fun l (i,(p,_,_,_),l') -> (i,p,l')::l)
	              complexe.open_links av_sites}::sol
	else 
	  let m2' = TSMap.add ts (k2+1) m2 in 
	  list_fold 
	    (fun (i',l') (sol) -> 
	      if i'>n or not (conc.good_binding (i,l) (i',l'))
	      then 
		sol 
	      else 
		let k=try (IntMap.find i' m) with Not_found -> 0 in 
		if k>=2 
		    or (k=1 && ((i=i' && l<>l') 
				  or (try (IntMap.find i' a <> l') with Not_found -> false)))
		then 
		  {card = complexe.card;
		    agents = complexe.agents;
		    available_sites = [];	     
		    links = complexe.links;
		    open_links = 
		    List.fold_left 
		      (fun l (i,(p,_,_,_),l') -> (i,p,l')::l)
	              complexe.open_links av_sites}::sol
		else 
		  {card = (conc.weigth_of_id i')*complexe.card;
		    agents = {isid=i';path=l::p}::complexe.agents;
		    available_sites = 
		    list_fold 
		      (fun s sol ->
			if s=l' then sol 
			else (i',(l::p,IntMap.add i' (k+1) m,
				  m2',
				  IntMap.add i l a),s)::sol)
		      (List.rev (conc.get_sites i')) (av_sites) ;
		    links = ((i,p,l),(i',l::p,l'))::complexe.links;
		    open_links = complexe.open_links}::sol)
	    (conc.which_binding (i,l)) sol
  in 
  let deal_with complexe (sol,(closed_sol,open_sol)) n (nclose,nopen) = 
    match complexe.available_sites with 
      [] -> sol, 
	if complexe.open_links = [] && test complexe conc 
	then 
	  (let nclose = nclose + complexe.card in 
           let _ = log nclose in 
             (store complexe closed_sol,open_sol,nclose,nopen) )
	   else
	       
								    if complexe.open_links <> [] then closed_sol,store complexe open_sol,nclose,nopen+complexe.card
								    else closed_sol,open_sol,nclose,nopen
    | _ -> 
	enlarge complexe (close complexe sol n) n (nclose,nopen),(closed_sol,open_sol,nclose,nopen) in 
  let rec aux1 k (closed_sol,open_sol,nclose,nopen)  = 
    
    if k = 0 then closed_sol,open_sol,nclose,nopen
    else 
	let rec aux (sol,(closed_sol,open_sol,nclose,nopen)) n = 
	  match sol with [] -> closed_sol,open_sol,nclose,nopen
	    |	t::q -> 
		  aux 
		    (deal_with t (q,(closed_sol,open_sol)) n (nclose,nopen))
		    n 
	in 
	  aux1 (k-1) 
	    (aux 
	       ([complexe_of_gspec (IntMap.find k conc.g_species)],(closed_sol,open_sol,nclose,nopen)) 
	       k)
  in 
  let rep1,rep2,rep3,rep4 = aux1 (conc.number) ([],[],0,0)  in 
    if (!Config_complx.sort_complexes)
    then
    (sort_complexe rep1 conc,sort_complexe rep2 conc,rep3,rep4,messages)
  else 
    (rep1,rep2,rep3,rep4,messages)
      
      
