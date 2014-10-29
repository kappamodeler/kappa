(* 27/10/2014 *)
(* reaction_computation *)
(* Jerome Feret pour OpenKappa *)
(* ode_computation.ml *)

open Data_structures
open Pb_sig 
open Tools 
open Output_contact_map
open Ode_print_sig 
open Rooted_path 
open Annotated_contact_map 
open Fragments 
open Arithmetics 
open Ode_print 
open Views 
open Error_handler 

let debug = false
let log_step = false
let memory = true
let allow_cycle_in_lhs = true

let error i = 
  unsafe_frozen (Some "") (Some "Complx") (Some "Ode_computation.ml") None (Some ("line  "^(string_of_int i))) (fun () -> raise Exit)

let error_ext i s m = 
  unsafe_frozen m (Some "Complx")  (Some "fragments.ml") s (Some ("line  "^(string_of_int i))) (fun () -> raise Exit)


let dump_line i = 
  if debug then 
    begin
      print_string "line ";
      print_int i;
      print_newline ()
    end

(* Temporary Linking *)
module New_F = New_Fragment

module F = New_F 

let get_denum = F.get_denum 
let complete_subspecies = F.complete_subspecies 
let empty_species = F.empty_species 
let empty_fragment = F.empty_fragment
let is_empty_fragment = F.is_empty_fragment 
let is_empty_species = F.is_empty_species 
let canonical_form = F.canonical_form
let iter_views = F.iter_views 
let fold_views = F.fold_views 
let iter_views_in_species = F.iter_views_in_species 
let scan_views_in_species = F.scan_views_in_species 
let plug_views_in_subspecies = F.plug_views_in_subspecies
let split_tp_list = F.split_subspecies 
let get_views_from_agent_id = F.get_views_from_agent_id 
(*let get_neighbour = F.get_neighbour *)
let build_species = F.build_species  
let apply_blist_with_species = F.apply_blist_with_species
let remove_agent_in_species = F.remove_agent_in_species 
let merge = F.merge 
let split_subspecies = F.split_subspecies 
let empty_hash = F.empty_hash 
let check_compatibility = F.check_compatibility 
let is_agent_in_species = F.is_agent_in_species 
let print_species = F.print_species
let add_bond_to_subspecies = F.add_bond_to_subspecies 
let release_bond = F.release_bond_from_subspecies 
let print_fragment = F.pretty_print 
let root_of_species = F.root_of_species

module FragmentMap = F.FragMap 
module RootedFragmentMap = F.RootedFragMap
(* End of temporary linking *)




type posneg = Creation | Suppression

type update = 
    {target:int;
     rate:expr}

type bond = (string*string*string*string)

type 'subspecies subclass = 
    {
    bond:bond option;
    subspecies:'subspecies;
    fragment_extension:'subspecies list option ; (* TO DO simplify the hierarchy of list, smash with the next imbrication level in the field subclass of 'a classes *)
    agent_list:string list}
  
let init_subclass (b,c,d) = 
  {bond=b;
   subspecies= c;
   fragment_extension = None;
   agent_list= d}


type ('a,'b) classes = 
	{rule:'a rule_guard;
	agents_id:StringSet.t;
	guard_as_a_list:(b*bool) list option;
	guard_as_a_map:bool BMap.t option;
	roots: StringSet.t option;
        subclass:'b subclass list list option ; (* a subclass is 
						    a choice (list/an elmt per potential signature of the lens
						     of compositions (list/an elmt per subcomposant (delimited by dashed edges)
                                                     of choices (list/an elmt per potential completion) *)
	extended_passives:((string*string*string)*(string*string*string)) list;  
        intra_link:((string*string*string)*(string*string*string)) list;
	rate:expr option;
	rate_other:expr option }
      	



let empty_class g = 
  {rule = g;
   agents_id = StringSet.empty;
   guard_as_a_list = None;
   guard_as_a_map = None;
   roots = None;
   subclass = None;
   rate = None;
   rate_other = None;
   extended_passives = [];
   intra_link = [] }

let get_rule_of_class g = g.rule
let check x = 
  match x with 
    None -> error 50 
  | Some a -> a

let get_agents_id_of_class  g      = g.agents_id 
let get_guard_as_a_list_of_class g = check g.guard_as_a_list
let get_guard_as_a_map_of_class g  = check g.guard_as_a_map
let get_roots_of_class g = check g.roots 


let print_log s = 
  if log_step then (print_string s;print_newline ())




let compute_ode file_reaction file_ODE_perturbation file_ODE_contact file_stoc_contact file_stoc_rules file_ODE_covering file_ODE_covering_latex file_ODE_latex file_ODE_matlab file_ODE_matlab_aux file_ODE_matlab_size file_ODE_matlab_jacobian file_ODE_matlab_act file_ODE_matlab_obs file_ODE_matlab_init file_ODE_mathematica file_ODE_txt  file_alphabet file_obs file_obs_latex file_ODE_data_head file_data_foot file_ODE_data file_ODE_gplot file_ODE_png file_ODE_script file_XML ode_handler output_mode  prefix log pb pb_boolean_encoding subviews  auto compression_mode pb_obs  exp var_of_b  varset_empty varset_add build_kleenean print_kleenean (l,m) = 
  
 
  let n_perturbation = 
    1+(Mods2.IntMap.fold 
      (fun i _ sol -> max i sol)
      exp.Experiment.perturbations_unfun 0 )
  in 
  let prefix' = "-"^(fst prefix) in 
  let do_latex = !Config_complx.do_dump_latex in 
  let good_mode a b = 
    (file_ODE_mathematica<>"" && b=MATHEMATICA) or 
    (file_ODE_matlab<>"" && b=MATLAB) or 
    (do_latex && b=LATEX) 
      or b=DATA or b = REACTIONS 
  in 
  let file_ODE_latex,file_obs_latex= 
    if do_latex 
    then file_ODE_latex,file_obs_latex
    else "","" in 
  let f mode file = 
    if good_mode output_mode mode
    then
      Some 
	begin
	  if file ="" 
	  then 
	    {print_string = (fun _ -> ());
	      print_float = (fun _ -> ());
	      print_int = (fun _ -> ());
	      chan = [];
	      print_newline = (fun _ -> ())}
	  else 
	    let _ = print_string file in 
	    let _ = print_newline () in 
	    let chan = open_out file  in
	    {print_string = Printf.fprintf chan "%s"  ;
	      print_float = (fun x -> Printf.fprintf chan "%s" (Float_pretty_printing.exact_string_of_float x)) ;
	      print_int = Printf.fprintf chan "%d" ;
	      chan = [chan];
	      print_newline = (fun () -> Printf.fprintf chan "\n")}
	end
    else None in

  let print_reaction = f REACTIONS file_reaction in 
  let print_latex_obs = f LATEX file_obs_latex in 
  let print_txt = f REACTIONS file_alphabet  in 
  let print_kappa = f REACTIONS file_obs in 


  let cpb = match pb.Pb_sig.intermediate_encoding with 
    Some cpb -> cpb
  | None -> error 178 in

  let sites_of_agent x = 
    try 
      StringMap.find x cpb.Pb_sig.cpb_interface_of_agent 
    with 
      Not_found -> error 202 in 
  let print_ODE_perturbation = 
     {dump = None;
     data = None;
     txt = None;
     kappa = None;
     mathematica = None;
     latex = None;
     matlab = None; 
     matlab_aux = None;
     matlab_jacobian = None;
     matlab_size = None ;
     matlab_activity = None;
     matlab_obs = None ;
     matlab_init = None ; 
     reactions = None 
     } in

  let print_ODE_jacobian = 
    {dump = None;
     data = None;
     txt = None;
     kappa = None;
     mathematica = None;
     latex = None;
     matlab = None;
     matlab_aux = None;
     matlab_jacobian = None; 
     matlab_size = None ;
     matlab_activity = None;
     matlab_obs = None ;
     matlab_init = None; 
     reactions = None} 
  in


 let print_ODE_act = 
    {dump = None;
     data = None;
     txt = None;
     kappa = None;
     mathematica = None;
     latex = None;
     matlab = None;
     matlab_aux = None;
     matlab_size= None;
     matlab_jacobian= None;
     matlab_activity = None; 
     matlab_obs = None;
     matlab_init = None ;
     reactions = None } 
 in
   
 let print_ODE_matlab_activity = print_ODE_act in 

 let print_ODE_matlab_obs = 
    {dump = None;
     data = None;
     txt = None;
     kappa = None;
     mathematica = None;
     latex = None;
     matlab = None;
     matlab_aux = None;
     matlab_size= None;
     matlab_jacobian= None;
     matlab_activity = None;
     matlab_obs = None;
     matlab_init = None ;
     reactions = None
    } 
 in
		 
  let print_ODE = 
     {dump = None ;
     txt = None ;
     data = None; 
     kappa = None ;
     mathematica = None;
     latex = None;
     matlab = None;
     matlab_aux = None;
     matlab_jacobian = None;
     matlab_size = None;
     matlab_activity = None;
     matlab_obs = None;
     matlab_init = None;
     reactions = None
     }
  in
  let print_ODE_main = 
    {dump = None ;
     txt = None ;
     data = None; 
     kappa = None ;
     mathematica = None;
     latex = None;
     matlab = None;
     matlab_aux = None;
     matlab_jacobian = None;
     matlab_size = None;
     matlab_activity = None;
     matlab_obs = None;
     matlab_init = None;
     reactions = None
    }
  in

  let print_ODE_aux = 
    {print_ODE with matlab_size = None ; matlab_jacobian = None ; matlab = None ; matlab_aux = None ; matlab_init = None } in

  let print_ODE_mathematica = 
    {print_ODE_main with matlab = None} in 
 
  let print_ODE_latex = 
     {dump = None;
      txt = None;
      data = None;
      kappa = None;
      mathematica = None;
      latex = None;
      matlab = None;
      matlab_aux = None;
      matlab_jacobian = None;
      matlab_size = None;
      matlab_activity = None;
      matlab_obs = None;
      matlab_init = None;
      reactions = None } in

  let print_ODE_matlab = 
    {dump = None;
      txt = None;
      data = None;
      kappa = None;
      mathematica = None;
      latex = None;
      matlab = None;
      matlab_aux = None;
      matlab_jacobian = None;
      matlab_size = None;
      matlab_activity = None;
      matlab_obs = None;
      matlab_init = None;reactions=None } in

  let print_ODE_matlab_aux = 
    {dump = None;
      txt = None;
      data = None;
      kappa = None;
      mathematica = None;
      latex = None;
      matlab_aux = None;
      matlab = None;
      matlab_jacobian = None ;
      matlab_size = None;
      matlab_activity = None;
      matlab_obs = None;
      matlab_init = None;
      reactions=None } in
 
 let print_ODE_matlab_size = 
    {dump = None;
      txt = None;
      data = None;
      kappa = None;
      mathematica = None;
      latex = None;
      matlab_aux = None;
      matlab = None;
      matlab_jacobian = None ;
      matlab_size = None;
      matlab_activity = None;
      matlab_obs = None;
      matlab_init = None;reactions=None } in

 

  let print_only_data = 
    {dump = None;
      txt = None;
      data = None;
      kappa = None;
      mathematica = None;
      latex = None;
      matlab= None;
      matlab_aux = None;
      matlab_jacobian = None ;
      matlab_size = None;
      matlab_activity = None;
      matlab_obs = None;
      matlab_init = None;reactions=None } in 

  let print_debug = 
    {dump = Some stdprint ;
     txt=None;
     data = None;
     kappa=None;
     mathematica=None;
     latex = None;
     matlab = None;
     matlab_aux = None;
     matlab_jacobian = None ;
     matlab_size = None;
     matlab_activity = None;
     matlab_obs = None;
     matlab_init = None;reactions=None } 
  in
  let print_obs = 
    { dump = None ;
      data = None ;
      kappa = print_kappa ;
      mathematica = None;
      latex = None;
      txt = print_txt  ;
      matlab = None;
      matlab_aux = None;
      matlab_jacobian = None;
      matlab_size = None ;
      matlab_obs = None ;
      matlab_activity = None ;
      matlab_init = None;reactions=None } 

  in 
  let print_obs_latex = 
    { dump = None;
      data=None;
      kappa = None; 
      mathematica = None;
      latex = print_latex_obs;
      txt = None;
      matlab = None;
      matlab_aux = None ;
      matlab_jacobian = None ;
      matlab_size = None ;
      matlab_obs = None ;
      matlab_activity = None;
      matlab_init = None;reactions=None }
  in
  let print_ODE_matlab_init = 
     { dump = None;
      data=None;
      kappa = None; 
      mathematica = None;
      latex = None;
      txt = None;
      matlab = None;
      matlab_aux = None ;
      matlab_jacobian = None ;
      matlab_size = None ;
      matlab_obs = None ;
      matlab_activity = None;
      matlab_init = None;
      reactions=None }
  in
  let print_reactions = 
     { dump = None;
      data=None;
      kappa = None; 
      mathematica = None;
      latex = None;
      txt = None;
      matlab = None;
      matlab_aux = None ;
      matlab_jacobian = None ;
      matlab_size = None ;
      matlab_obs = None ;
      matlab_activity = None;
      matlab_init = None;reactions=print_reaction }
  in 
  let _ = dump_line 429 in  
  let is_access = 
    match pb.unreachable_rules with 
      None -> (fun x -> true)
    | Some a -> (fun x -> not (RuleIdSet.mem x a)) in 

  let a = pb_boolean_encoding in 

  
  let clean rs =  
    (* to remove unreachable rule *)
    let rs = 
      {rs with rules = 
	List.rev 
	  (
	List.fold_left  
	  (fun sol x -> 
	    let lab = List.filter (fun a -> not a.r_clone && ((try None  = IntMap.find  a.Pb_sig.r_simplx.Rule.id  pb_obs with _ -> false)  or (is_access a))) x.labels in
	    if lab = [] then sol
	    else ({x with labels = lab})::sol)
	  [] rs.rules)} in 
    rs in

    
  let _ = dump_line 453 in 

  let simplify rs = 
    (* when a passive species is not tested, it can be written in the binding type *)
    let a = rs.Pb_sig.passive_species in 
    let pass,forget_agents = 
      List.fold_left 
	(fun (pass,forget_agents) ((a,b,c),(d,e,f))->
	  if List.exists 
	      (fun rule -> 
		List.exists 
		  (fun test ->
		    match test with 
			M((a',b',_),_),_ when (a,b)=(a',b') -> true
		      | B(a',b',c'),_ | AL((a',b',c'),_),_ 
		      | L((a',b',c'),_),_  when (a,b)=(a',b') && c'<>c -> true
		      | L(_,(a',b',c')),_  when (a,b)=(a',b') && c'<>c -> true 
		      | _ -> false
		    ) rule.Pb_sig.injective_guard)
	      rs.Pb_sig.rules
	  then 
	    ((a,b,c),(d,e,f))::pass,forget_agents
	  else
	    (pass,StringSet.add a forget_agents))
	([],StringSet.empty) a in
    {rs with 
      Pb_sig.passive_species = pass ;
      Pb_sig.rules = 
      List.map 
	(fun r -> {r 
	      with Pb_sig.injective_guard = 
		List.fold_left
		  (fun l x  ->
		    match x with 
		      H(a,_),_ | 
		      B(a,_,_),_ | AL((a,_,_),_),_ | M((a,_,_),_),_ -> 
			if not (StringSet.mem a forget_agents)
			then x::l
			else l
		    | L((aid,atype,asite),(bid,btype,bsite)),bool -> 
			if not (StringSet.mem aid forget_agents) && 
			  not (StringSet.mem bid forget_agents)
			then 
			  x::l
			else
			  if bool then 
			    let l = 
			      if not (StringSet.mem aid forget_agents )
			      then 
				(AL((aid,atype,asite),(btype,bsite)),true)::l
			      else
				l
			    in
			    let l = 
			      if not (StringSet.mem bid forget_agents)
			      then
				(AL((bid,btype,bsite),(atype,asite)),true)::l
			      else
				l
			    in
			    l
			  else
			    l
			      
		    | _ -> l)
		      [] r.Pb_sig.injective_guard
		      
	      })	
	rs.Pb_sig.rules } in 


    
 


(******************************************************)
(* WE REMOVE DEAD RULES, AND SIMPLIFY PASSIVE SPECIES *) 
(******************************************************)
  
 
  let system = List.map (fun x -> simplify (clean x)) a.Pb_sig.system in 

  let flag_map = (StringMap.empty,IntMap.empty) in 
  let flag_map = 
    List.fold_left  
      (fun flag_map x -> 
	 try 
	   let label = List.hd (List.hd x.Pb_sig.rules).Pb_sig.labels in 
	   let rule_id = name_of_rule label in 
	   let key = label.Pb_sig.r_simplx.Rule.id in 
	   let flag_map = (StringMap.add rule_id key (fst flag_map),IntMap.add key rule_id (snd flag_map)) in 
	     flag_map
	 with 
	       _ -> flag_map
      )
      flag_map system 
  in 
  let flag_map_inv = fst flag_map in 
  let pb_obs' = 
      IntMap.map 
	(fun x -> 
	   match x with None -> None
	     | Some r -> 
		 begin 
		   try (Some (StringMap.find r (fst flag_map)))
		   with Not_found -> None
		 end)
	pb_obs in 
  

  let obs_map_inv = 
    snd 
      (IntMap.fold 
	 (fun i j (k,map) -> 
            let map  = 
	      (try StringMap.add (IntMap.find i (snd flag_map)) k map 
	       with 
		   Not_found -> 
                     match j 
                     with None -> map
                       | Some j -> 
                           try 
                             StringMap.add 
                               ("["^(IntMap.find j (snd flag_map))^"]") 
                               k 
                               map
                          with Not_found -> map
              )
            in (k+1,map))
         pb_obs' (1,StringMap.empty))
  in 

 
  let _ = pprint_ODE_head print_ODE print_ODE_matlab_obs print_ODE_matlab_activity print_ODE_perturbation file_ODE_matlab file_ODE_matlab_aux file_ODE_matlab_jacobian file_ODE_matlab_size file_ODE_matlab_act file_ODE_matlab_obs file_ODE_perturbation exp obs_map_inv flag_map_inv in 

        
  let _ = print_log "COMPUTE ANNOTATED CONTACT MAP" in

  (****************************)
  (* WE FETCH THE CONTACT MAP *)
  (****************************)

  let contact_internal,contact =
    match pb.Pb_sig.contact_map 
    with 
      None -> error 352 
    | Some a -> 
    begin 
      a,(fun x -> 
	try 
	  String2Map.find x a.Pb_sig.link_of_site
	with
	  Not_found -> 
	    [])
    end
  in 
  

  (*******************************************************************)
  (* WE COMPUTE THE ANNOTATED CONTACT MAP FROM THE SIMPLIFIED SYSTEM *)
  (*******************************************************************)
  
  let pre_annotated_contact_map = 
    match compression_mode 
      with 
	Flat -> upgrade (compute_annotated_contact_map_in_flat_mode system cpb contact ) cpb
      |	Compressed -> compute_annotated_contact_map_in_compression_mode system cpb contact 
      |	Approximated -> upgrade(compute_annotated_contact_map_in_approximated_mode system cpb contact ) cpb
      | Stoc -> Annotated_contact_map_stoc.compute_annotated_contact_map_in_stoc_mode system pb cpb contact_internal
    in 
  

  (*************************************************)
  (* WE REMOVE CLASSES THAT ARE INCLUDED IN OTHERS *)
  (*************************************************)
  
  let annotated_contact_map  = 
    {subviews=
      StringMap.map
	(fun b -> 
	  let rec aux a b = 
	    match a with [] -> b
	    | t::q -> 
		if List.exists (fun a' -> sub_template t a') q or 
		  List.exists (fun a' -> sub_template t a') b 
		then aux q b
		else aux q (t::b)
	  in aux b []
	    )
	
	(fst pre_annotated_contact_map) ;
      solid_edges = snd pre_annotated_contact_map}
  in 
  (************************************************************)
  (* WE GENERATE THE DOT OUTPUT FOR THE ANNOTATED CONTACT MAP *)
  (************************************************************)

  let _ = 
    match compression_mode 
    with 	
      | Flat | Compressed | Approximated ->
        dump_contact_map_in_dot 
          cpb.cpb_interface
          (match pb.contact_map
           with Some l -> (Some l.live_agents)
	     | None -> None)
          (match pb.contact_map 
           with Some l -> l.relation_list
             | None -> [])
          (fun a b -> not (String22Set.mem (a,b) annotated_contact_map.solid_edges))
          file_ODE_contact
      | Stoc -> 
        let _ = 
           Annotated_contact_map_stoc.output_renamed 
             file_stoc_rules
             Tools.string_txt
             (Some "()")
             pb 
             annotated_contact_map
             var_of_b 
             varset_empty 
             varset_add 
             build_kleenean 
             print_kleenean 
        in 
        let _ = 
          dump_stoc_contact_map_in_dot 
            cpb.cpb_interface
            (match pb.contact_map
             with Some l -> (Some l.live_agents)
	       | None -> None)
            (match pb.contact_map 
             with Some l -> l.relation_list
               | None -> [])
            (fun a b -> 
              not (String22Set.mem (a,b) annotated_contact_map.solid_edges))
            (StringMap.map (List.map (fun x -> x.kept_sites)) annotated_contact_map.subviews )
            file_stoc_contact
        in 
        ()
  in 

  (***************************************)
  (* WE DUMP THE COVERINGS IN TXT FORMAT *)
  (***************************************)
  
  let _ = 
    if file_ODE_covering <> "" && compression_mode<>Stoc
    then 
      let chan = open_out file_ODE_covering in 
      let print_string x = Printf.fprintf chan "%s" x in
      let _ = 
	  StringMap.iter 
	  (fun a l -> 
	    print_string "Agent: ";
	    print_string a ;
	    print_string "\n";
	    List.iter 
	      (fun s -> 
		print_string " Covering class: ";
		let _ = 
		  StringSet.fold
		    (fun a bool -> 
		      let _ = if bool then print_string ", " in
		      let _ = print_string a in
		      true)
		    s.kept_sites  false in
		()) l;
	    print_string "\n")
	  annotated_contact_map.subviews in
      let _ = close_out chan in 
      () in
  


  (***************************************)
  (* WE DUMP THE COVERINGS IN LATEX FORMAT *)
  (***************************************)
  


    let _ = 
    if file_ODE_covering_latex <> "" && compression_mode <> Stoc
    then 
      let chan = open_out file_ODE_covering_latex in 
      let print_string x = Printf.fprintf chan "%s" x in
      let _ = print_string "\\begin{enumerate}\n" in 
      let _ = 
	  StringMap.iter 
	  (fun a l -> 
	    print_string "\\item agent: ";
	    print_string (Latex.string_of_agent_name a);
	    print_string "{}";
	    print_string "\\begin{itemize}";
	    List.iter 
	      (fun s -> 
		print_string "\\item \\{";
		let _ = 
		  StringSet.fold
		    (fun a bool -> 
		      let _ = if bool then print_string Latex.site_sep in
		      let _ = print_string (Latex.string_of_site_name a) in
		      let _ = print_string "{}{}" in 
		      true)
		    s.kept_sites  false in
		print_string "\\}";
		()) l;
	    print_string "\\end{itemize}\n")
	  annotated_contact_map.subviews in
      let _ = print_string "\\end{enumerate}\n" in 
      let _ = close_out chan in 
      () in

    match compression_mode 
    with Stoc -> None,(l,m)
      | _ -> 
        begin 
          let _ = print_log "COMPUTE FRAGMENTS" in
          let _ = 
            if debug   then dump_template print_debug annotated_contact_map in 
          
  
  (************************************)
  (* WE CREATE THE LIST OF (SUB)VIEWS *)
  (************************************)

  let views_list  = translate_classes_into_views ode_handler subviews annotated_contact_map in 
  

  (*****************************)
  (* WE DUMP THE LIST OF VIEWS *)
  (*****************************)

   let _ = 
    if debug   then 
      let _ = print_string "VIEWS\n\n" in 
      let _ = 
	List.fold_left
	  (fun i view  -> 
	    let _ = print_newline () in 
	    let _ = print_string "VIEW: " in
	    let _ = print_string (string_of_int i) in 
	    let _ = print_newline () in 
	    let _ = print_string "AGENT: " in
	    let _ = print_string (agent_of_view view) in
	    let _ = print_newline () in 
	    let _ = print_string "SITES: " in
	    let _ = 
	      StringSet.iter  (fun x-> print_string x;print_string ", ") (interface_of_view view).kept_sites  in  
	    let _ = print_newline () in 
	    let _ = 
	      List.iter 
		(fun (b,bool) ->
		  print_string (string_of_b  (ode_handler.b_of_var b));
		  print_string (if bool then "TRUE" else "FALSE");
		    print_newline ())
		(valuation_of_view view) 
	    in (i+1))
	  1 views_list 
      in () in
  

  let create_fragments_hashtable ode_handler print  pb  = 
    let hash_tp_list,dump,size,get_fragment  = 
      let n = ref 1 in
      let map = ref FragmentMap.empty in
      let conv = ref IntMap.empty in 
      let size () = (!n)-1 in 
      (let f (x,aut) = 
	let x'=x in 
	try (FragmentMap.find x' (!map))
	with Not_found -> 
	  let rep = (!n) in
	  let _ = if debug then 
            let _ = print_log ("NEW FRAGMENT: "^(string_of_int (!n))^"\n VIEWS:\n") in
   	    let _ = iter_views
		(fun i -> 
		  let _ = print_log (string_of_int i) in 
		  (*let view = view_of_tp_i i in
		  let _ = 
		    List.iter 
		      (fun (b,bool) ->
			print_string (string_of_b  (ode_handler.b_of_var b));
			print_string (if bool then "TRUE" else "FALSE");
			print_newline ())
		      (valuation_of_view view) 
		  in*) ())
		x' in 
	    let _ = print_log "\n" in () in 
	  let _ = n:= (!n)+1 in
	  let _ = map:=FragmentMap.add x' (rep,aut) (!map) in
	  let _ = conv:= IntMap.add rep x' (!conv) in 
            (rep,aut)
      in f),
      (let dump view_data_structure fmap  = 
	let _ = 
	    if !Config_complx.trace_rule_iteration 
	    then 
	      begin 
		print_string prefix';
		print_string "  ";
		print_int (size ());
		print_string " fragments\n";
		print_string prefix' ;
		print_string "Start dumping fragment definitions";
		print_newline ()
	      end  in 
	  
	let l = 
	  FragmentMap.fold
	    (fun l (n,aut) list -> 
	       ((n,l)::list))
	    (!map) [] in 
	let l = List.sort compare l in 
	let _ = pprint_string print_obs_latex (Latex.init_sep) in
	let bool = 
	  List.fold_left
	    (fun bool (n,expr) -> 
	       let _ = 
		 if bool 
		 then pprint_string print_obs_latex (Latex.sep) in 
	       let _ = 
		  print_fragment 
		    print_obs_latex 
		    expr 
		    string_latex 
		    ode_handler 
		    view_data_structure 
		    (fun a b -> keep_this_link a b annotated_contact_map)
		    (Some "")
		    true 
	       in
		 true)
	    false  
	    l 
	in 
	let _ = if bool then pprint_string print_obs_latex Latex.final_sep in 	  	  
	let _ =
	  List.fold_left
	    (fun bool (n,expr) -> 
	       let f print_obs expr = 
		   ((print_fragment 
		       print_obs
		       expr
		       string_txt 
		       ode_handler 
		       view_data_structure 
		       (fun a b -> keep_this_link a b annotated_contact_map) 
		       (Some "()")
		       false
                    ):unit) 
	       in
	       let print_obs = {print_obs with data = None} in 
	       let _ = 
		 pprint_obs_in_reaction 
		   print_obs 
		   (f print_obs) 
		   n
		    expr 
		   pb in 
		 true)
	    false  
	    l in  
	let _ =
	  List.fold_left
	    (fun bool (n,expr) -> 
	       let f print_obs expr = 
		 ((print_fragment 
		     print_obs
		     expr
		     string_txt 
		     ode_handler 
		     view_data_structure 
		     (fun a b -> keep_this_link a b annotated_contact_map) 
		     (Some "()")
		     false
                  ):unit) 
	       in
	       let _ = 
		 pprint_obs_in_reaction  
		   print_only_data
		   (f print_only_data) 
		   n
		   expr 
		   pb in 
		true)
	    false  
	    (List.rev l)  
	    
	in
	  () in dump),
      (size) ,
      (let f k = 
         try 
           IntMap.find k (!conv)
         with 
             Not_found -> error 896
       in f)
      
       
    in
      hash_tp_list,dump,size,get_fragment in 
    

  (*****************************************)
  (* WE CREATE THE HASHTABLE FOR FRAGMENTS *)
  (*****************************************)

  let hash_fragment,dump,size,get_fragment  = create_fragments_hashtable ode_handler print_obs pb in 
  let hash_subspecies x = hash_fragment (canonical_form x) in 
    (* hash_subspecie maps each subspecies to their fragment identifier. *)
    (* dump dump the content of the hashtable *)
  
 
  (*******************************************)
  (* WE CREATE THE DATA-STRUCTURES FOR VIEWS *)
  (*******************************************)

  let views_data_structures,_  = 
    gather ode_handler annotated_contact_map views_list  in 

  let view_of_tp_i x = view_of_tp_i x views_data_structures.interface_map in 
  (* map a view id to a view *)

  let agent_of_tp_i x = agent_of_tp_i x views_data_structures.interface_map in 
  (* map a view id to an agent type *)

  let bool_of_tp_i x = bool_of_tp_i x views_data_structures.interface_map in
  (* map a view id to a boolean valuation *)



  (***************************************************************************)
  (* WE COMPUTE WHICH VIEWS CAN BE PLUGGED FOR A GIVENT PAIR AGENT/SITES SET *)
  (***************************************************************************)

  (* this map associates each agent/site sets to the list of ids of the views that match this agent/set of sites (exactly) *)
  let pre_agent_to_int_to_nlist = 
      Arraymap.fold 
	(fun n view sol ->
	  let agent  = agent_of_view view in
	  let l = 
	    List.rev 
	      (StringSet.fold 
		 (fun x l -> x::l) 
		 (interface_of_view view).kept_sites []) in
	  let old = 
	    try StringMap.find agent sol 
	    with Not_found -> StringListMap.empty 
	  in
	  let old2 = 
	    try StringListMap.find l old
	    with Not_found -> [] in
	  StringMap.add agent  
	    (StringListMap.add l (n::old2) old) 
	    sol)
	views_data_structures.interface_map StringMap.empty in

  (* this map associates each agent/site sets to the list of ids of a maximal family of compatible views of that agent with that contains at least these sites *)
  let agent_to_int_to_nlist_compatibility = 
      StringMap.map 
	(fun map -> 
	  let add l n map = 
	    try 
	      StringListMap.find l map;map 
	    with 
	      Not_found -> 
		StringListMap.add l n map in 
	  StringListMap.fold 
	    (fun l n map -> 
	      List.fold_left
		(fun map l' -> add l' n map)
		map 
		(sublist l))
	    map StringListMap.empty  )
	pre_agent_to_int_to_nlist  in 

  let pre_agent_to_int_to_nlist = 
      StringMap.map 
	(fun map -> 
	  let add l n map = 
	    StringListMap.add 
	      l 
	      (let old = 
		try 
		  StringListMap.find l map
		with Not_found -> IntSet.empty in 
	      List.fold_left
		(fun set a -> IntSet.add a set)
	        old n)
	      map
	  in

	  StringListMap.fold 
	    (fun l n map -> 
	      List.fold_left
		(fun map l' -> add l' n map)
		map 
		(sublist l))
	    map StringListMap.empty  )
	pre_agent_to_int_to_nlist in


(* this map associates each agent/site sets to the list of ids of the compatible views of that agent with that contains at least these sites *)
  let agent_to_int_to_nlist = 
    StringMap.map 
      (StringListMap.map IntSet.elements)
      pre_agent_to_int_to_nlist  in 

  (****************************************************************)
  (* WE DUMP WHICH VIEWS CAN BE PLUGGED TO COVER A GIVEN SITE SET *)
  (****************************************************************)

   let _ =
     if debug then 
       let _ = 
	 print_string "AGENT_to_INT_to_nlist\n" in
       StringMap.iter 
	 (fun s map ->
	   print_string s;
	   print_newline ();
	   StringListMap.iter 
	     (fun s n -> 
	       List.iter (fun x->print_string x;print_string ",") s;
	       print_newline ();
	       List.iter (fun n -> print_int n;print_string ",") n;
	       print_newline ())
	     map)
	 agent_to_int_to_nlist 
    in 
   let _ =
     if debug then 
       let _ = 
	 print_string "AGENT_to_INT_to_nlist_compatibility\n" in
       StringMap.iter 
	 (fun s map ->
	   print_string s;
	   print_newline ();
	   StringListMap.iter 
	     (fun s n -> 
	       List.iter (fun x->print_string x;print_string ",") s;
	       print_newline ();
	       List.iter (fun n -> print_int n;print_string ",") n;
	       print_newline ())
	     map)
	 agent_to_int_to_nlist_compatibility 
    in 

   
 
  
   let get_denum0,get_denum1,get_denum2 = get_denum (agent_to_int_to_nlist,view_of_tp_i,ode_handler)   in 
   let get_denum = 
     match !Config_complx.ode_memoization_level 
     with 
       2 -> get_denum2
     | 1 -> get_denum1
     | _ -> get_denum0  in 

    (* The following function maps a bonds to a maximal class of compatible extensions of it (according the views and the annotated contact map) *)
   let get_denum_handling_compatibility = get_denum true  in 
  
    (* The following function maps a bonds to all compatible extensions (according to the views and the annotated contact map *)
    let get_denum = get_denum false in



    (* Whether a link is solid, or not *)
    let keep_this_link a b = keep_this_link a b annotated_contact_map in

   
    let complete_subspecies_handling_compatibility = 
       complete_subspecies 
	(pending_edges,
	 view_of_tp_i,
	 keep_this_link,
	 get_denum_handling_compatibility) in 

 (* This function provides the list of extension of a subspecies according to the annotated contact map *)
    let complete_subspecies = 
      complete_subspecies 
	(pending_edges,
	 view_of_tp_i,
	 keep_this_link,
	 get_denum) in 

   
    let _ = print_log "COMPUTE ACTIVITY" in 

    let get_fragment_extension x = 
      match x.fragment_extension 
      with None -> error 419
      |	Some a -> a in 
  

    let activity_map = IntMap.empty in 
    let rate_map = IntMap.empty in 
    let system = if compression_mode = Stoc then [] else system in 
    let activity = 
      List.fold_left  
	(fun (
	   (activity_map:expr IntMap.t),
	      (rate_map:float IntMap.t),
	      (flag_map:int StringMap.t * string IntMap.t )) 
	   x -> 
	     let rule_key,rule_id,flag,flag_map  =
	       try 
		 let label = List.hd (List.hd x.Pb_sig.rules).Pb_sig.labels in 
		 let rule_id = name_of_rule label in 
		 let flag = ltrim label.r_id in 
		 let key = label.Pb_sig.r_simplx.Rule.id in 
		 let flag_map = (StringMap.add rule_id key (fst flag_map),IntMap.add key rule_id (snd flag_map)) in 
	      key,rule_id,flag,flag_map
	       with 
		 _ -> 0,"EMPTY","EMPTY",flag_map
	     in 
	     
	  let _ = 
	    if debug then 
	      let _ = dump_line 1082 in 
	      let _ = print_string rule_id in 
	      let _ = print_newline () in 
	      () 
		
	  in 
	  
	  let control = x.Pb_sig.control in 
	  let passives = x.Pb_sig.passive_species in 
	
	  let specie_of_id y = 
	    try 
	      StringMap.find y x.Pb_sig.specie_of_id
	    with 
	      Not_found -> 
		y in
	  if rule_id = "EMPTY" 
	  then activity_map,rate_map,flag_map
	  else 
	    let _ = 
	      if !Config_complx.trace_rule_iteration && rule_id <> "EMPTY"
	      then 
		begin 
		  print_string prefix';
		  print_string "Start translating rule: ";
		  print_string rule_id;
		  print_string "(";
		  print_int rule_key;
		  print_string ")";
		  print_newline ();
		  print_string prefix';
		  print_string "  ";
		  print_int (size ());
		  print_string " fragments so far";
		  print_newline ();
		end  
	    in
	    let expr_handler = 
	      {hash_subspecies=hash_subspecies;
	       get_denum_handling_compatibility=get_denum_handling_compatibility;
	       get_bond=(fun x -> x.bond);
	       get_fragment_extension=get_fragment_extension} in 
	    let _ = dump_line 1143 in 
	    begin 
	      let _ = dump_line 1388 in 
	      let classes = 
		List.map  
		  (fun xx -> 
		    let passives = 
		      let passives_target = 
			List.fold_left 
			  (fun set ((a,_,_),_) -> StringSet.add a set)
			  StringSet.empty 
			  passives 
		      in 
		      List.fold_left 
			(fun passives (b,bool) -> 
			  match b,bool with 
			    L((a,a2,s),(a',a2',s')),true -> 
			      if not (StringSet.mem a passives_target) && not (StringSet.mem a' passives_target)
			      then 
				(((a,a2,s),(a',a2',s'))::passives)
			      else passives 
			  | _ -> passives)
			passives 
			xx.Pb_sig.injective_guard 
		    in 
                    let intra_link = 
                      List.fold_left 
			(fun intra (b,bool) -> 
			  match b,bool with 
			    L((a,a2,s),(a',a2',s')),true -> 
			      (((a,a2,s),(a',a2',s'))::intra)
			  | _ -> intra)
			[]
			xx.Pb_sig.injective_guard 
                    in 
                    let _ = 
                      if (not allow_cycle_in_lhs) && 
			
                        begin (*is there a cycle in the lhs of the rule ?*)
                          let agent_to_sites,contact = 
                            let fadd (a,s) (a',s') (agent_to_sites,contact) = 
                              let fadd1 a s agent_to_sites = 
                                let old = 
                                  try 
                                     StringMap.find a agent_to_sites 
                                  with 
                                    Not_found -> StringSet.empty 
                                in 
                                StringMap.add a (StringSet.add s old) agent_to_sites 
                              in 
                              let agent_to_sites = 
                                fadd1 a s (fadd1 a' s' agent_to_sites)
                              in 
                              let contact = 
                                String2Map.add (a,s) (a',s')
                                  (String2Map.add (a',s') (a,s) contact)
                              in 
                              agent_to_sites,contact 
                            in 
                            List.fold_left 
                              (fun x ((a,_,s),(a',_,s')) -> 
                                fadd (a,s) (a',s') x)
                                 (StringMap.empty,
                                  String2Map.empty)
                              intra_link 
                          in 
                          try (
	                    let add_relation x y (set,map) = 
	                      if x=y then raise Exit 
	                      else 
	                        let set' = String2Set.add x (String2Set.add y set) in
	                        let map' = 
	                             let old = 
		                       try Pb_sig.String2Map.find x map 
		                       with Not_found -> String2Set.empty in
	                             Pb_sig.String2Map.add x (String2Set.add y old) map in
	                        set',map' 
                            in
	                    let empty = String2Set.empty,Pb_sig.String2Map.empty in
                               let g (x,(a,s)) = 
                                 StringSet.fold   
	                           (fun s' -> 
                                     if s=s' 
                                     then (fun x -> x)
	                             else add_relation x (a,s') 
	                           )
	                           (StringMap.find a agent_to_sites)
                               in 
	                       let (set,map) = 
	                         Pb_sig.String2Map.fold 
	                           (fun x x' sol  -> g (x,x') (g (x',x) sol))
	                           contact
	                           empty
	                       in 
                               let rec aux next to_visit = 
                                 match next with 
                                   [] -> 
                                       begin
                                         if String2Set.is_empty to_visit 
                                         then 
                                           false
                                         else 
                                           let next = String2Set.min_elt to_visit 
                                           in 
                                             aux [next,None,String2Set.singleton next] (String2Set.remove next to_visit)
                                       end
                                 | (t,from,black)::q -> 
                                   let succ = 
                                     try 
                                       Pb_sig.String2Map.find t map 
                                     with 
                                       Not_found -> String2Set.empty
                                   in 
                                   let succ = 
                                     match from with None -> succ
                                     | Some a -> String2Set.remove a succ 
                                   in 
                                   aux 
                                           (String2Set.fold 
                                              (fun a l -> 
                                                if String2Set.mem a black
                                                then raise Exit 
                                                else 
                                                  (a,Some t,String2Set.add t black)::l
                                              )
                                              succ 
                                              q)
                                     (String2Set.diff to_visit succ)
                               in 
                               if String2Set.is_empty set then false
                               else 
                                 let start = String2Set.min_elt set in 
                                 aux 
                                   [start,None,String2Set.singleton start]
                                   (String2Set.remove start set)
                          )                
                          with Exit -> true
			    
			    
                        end
                      then 
                        error_ext 1457 None (Some "Cycles in lhs are not handled")
                    in 
		    StringSet.fold 
		      (fun x l ->
			let rec aux to_visit black set = 
			  match to_visit with 
			    [] -> set
			  | 
			      t::q -> 
			    let rec aux2 l to_visit black =
			      match l with 
				((a,a',_),(b,b',_))::q
				  when b=t && not (StringSet.mem a black)  
				  -> aux2 q (a::to_visit) (StringSet.add a black) 
			      | t::q -> aux2 q to_visit black 
			      | [] -> to_visit,black 
			    in
			    let tv,b = aux2 passives q black in
			       aux tv b (StringSet.add t set) in
			{(empty_class xx) with 
			  extended_passives = passives ;
			  intra_link = intra_link ; 
                          agents_id=aux [x] (StringSet.empty) (StringSet.singleton x)}::l)
		      (List.fold_left 
			 (fun set ((a',_,_),_) -> StringSet.remove a' set)
			 xx.Pb_sig.target passives) 
		      [])
		  x.Pb_sig.rules
	      in
	      let _ = dump_line 1438 in 
	      let classes = 
		List.map 
		  (fun x -> 
		    List.map 
		      (fun classe ->
			let xx  = get_rule_of_class classe in
			let cla = get_agents_id_of_class classe in 
			let blist =  
			  (List.filter 
			     (fun x -> 
			       match x with 
				 H(a,_),false | B(a,_,_),_ | AL((a,_,_),_),_ | M((a,_,_),_),_ when StringSet.mem a cla -> true 
			       | L((a,_,_),(b,_,_)),_ when StringSet.mem a cla or StringSet.mem b cla -> true
			       | _ -> false)
			     xx.Pb_sig.injective_guard) in
			let bmap =    
			  List.fold_left 
			    (fun map (b,bool) -> 
			      match b,bool with 
				B(_),_ | H(_),false 
			      | AL(_),_ | M(_),_ -> BMap.add ((*downgrade_b*) b) bool map 
			      | L((a,b,c),(d,e,f)),_ -> 
				if bool then 
				  BMap.add 
				    ((*downgrade_b*) (AL((a,b,c),(e,f))))
				    bool
				    (BMap.add 
				       ((*downgrade_b*) (AL((d,e,f),(b,c)))) 
				       bool
				       map)
				else
				  error 914 
			      | _ -> map) 
			    BMap.empty 
			    blist 
			in
			{classe with 
			  guard_as_a_list = Some blist; 
			   guard_as_a_map  = Some bmap}
			  
		      )
		      x) 
		  classes in 
	      let _ = dump_line 1482 in 
	      let classes = 
		List.map 
		  (fun x -> 
		    (List.map 
		       (fun cla -> 
			 let agentset = get_agents_id_of_class cla in 
			 let roots = 
			   List.fold_left
			     (fun set ((a,_,_),(a',_,_)) -> 
			       if StringSet.mem a' agentset 
			       then 
				 StringSet.remove a set 
			       else 
				 set )
			     agentset cla.extended_passives in
			 {cla with roots = Some roots}) x))
		  classes 
	      in
	      let _ = dump_line 1501 in 
	      let classes = 
		List.map 
		  (fun x -> 
		    (List.map 
		       (fun cla -> 
			 let rule = get_rule_of_class cla in
			 let blist = get_guard_as_a_list_of_class cla in
			 let bmap = get_guard_as_a_map_of_class cla in
			 let roots = get_roots_of_class cla in 
			 if not (StringSet.cardinal roots=1)
			 then error 944 
			 else
			   let root = StringSet.min_elt roots in
			   let _ = 
			     if debug 
			     then 
			       begin
				 print_string "ROOT: ";
				 print_string root;
				 print_newline ()
			       end in 
			   let rec aux l  sol = 
			     match l with 
			       [] -> sol
			     | (guard,to_visit_same_class,to_visit_other_class,black,prefix,same_class_agent,other_class)::q -> 
			       begin
				 match to_visit_same_class with 
				   [] -> 
				     begin
				       match to_visit_other_class 
				       with [] -> 
					 aux q (((init_subclass (guard,prefix,same_class_agent))::other_class)::sol)
				       | (g,a)::b -> 
					 aux (((g,[a],b,black,empty_species,[],(init_subclass (guard,prefix,same_class_agent))::other_class))::q) sol 
				     end
				       
				 | (a,from)::b -> 
				   if StringSet.mem a black  
				     or  
				       not 
				       (List.exists 
					  (fun x -> 
					    match x with 
					      H(x,_),b -> ((x=a && b)) 
					    | _  -> false) 
					  rule.Pb_sig.injective_guard) 
				   then 
				     (	
				       aux ((guard,b,to_visit_other_class,black,prefix,same_class_agent,other_class)::q) sol)
				   else
				     let restricted_blist = 
				       List.filter 
					 (fun (b,bool) -> 
					   match b,bool with 
					     H(b',_),false| B(b',_,_),_ | AL((b',_,_),_),_ | M((b',_,_),_),_ when b' = a -> true 
					   | L((b',_,_),(c',_,_)),_ when b'=a or c'=a -> true 
					   | _ -> false)
					 blist in
				     let bmap = 
				       BMap.fold 
					 (fun b bool sol -> 
					   match b,bool with 
					     H(b',_),false| B(b',_,_),_ | AL((b',_,_),_),_ | M((b',_,_),_),_ when b' = a -> BMap.add (downgrade_b b) bool sol 
					   | _ -> sol)
					 bmap
					 BMap.empty 
				     in   
				     let tp_list = 
				       compute_compatible_views_id 
					 blist 
					 restricted_blist 
					 bmap 
					 a 
					 (specie_of_id,agent_to_int_to_nlist_compatibility,view_of_tp_i,ode_handler)  in 
				     let _ = 
				       if debug
				       then 
					 begin 
					   print_string "COMPATIBLE_VIEWS";
					   print_string a;
					   print_newline ();
					   List.iter 
					     (fun x -> print_int x;print_newline ())
						  tp_list;
					   print_newline ();
					   print_string "BLIST\n";
					   List.iter 
					     (fun (x,bool) -> print_string 
					       (string_of_b x);print_string (if bool then "T\n" else "F\n"))
					     blist;
					   print_newline ();
					   print_string "RBLIST\n";
					   List.iter 
					     (fun (x,bool) -> print_string 
					       (string_of_b x);print_string (if bool then "T\n" else "F\n"))
					     restricted_blist;
					   print_newline ();
					   print_string "BMAP\n";
					   BMap.iter 
					     (fun x bool -> 
					       print_string 
						 (string_of_b x);print_string (if bool then "T\n" else "F\n"))
					     bmap;
					   print_newline ();
					   print_newline ()
					 end 
				     in 
				     let prefix,bound_agent_list_same_class,bound_agent_list_other_class = 
				       List.fold_left 
					 (fun 
					   (prefix,same,other) b 
					 ->
					   let f (a1,a2,a3,a5,a6) (prefix,same,other) =
 					     let a4 = 
						      let rec aux l = 
							match l with 
							  [] -> None 
                                                        | ((b4,b5,b6),(b1,b2,b3))::_ when (a1,a2,a3,a5,a6) = (b4,b5,b6,b2,b3) -> Some b1   
							| ((b1,b2,b3),(b4,b5,b6))::_ when (a1,a2,a3,a5,a6) = (b4,b5,b6,b2,b3) -> Some b1
							| _::q -> aux q 
						      in
						      aux (cla.intra_link)
					     in
					     (match a4 with 
					       None -> (prefix,same,other)
					     | Some a4 when StringSet.mem a4 black -> 
                                               (add_bond_to_subspecies prefix (a1,a3) (a4,a6),same,other)
					     | Some a4 -> 
					       if  not (List.exists (fun x -> x=(H(a4,a5),true)) rule.Pb_sig.injective_guard) then 
						 prefix,same,other
					       else
						 if keep_this_link 
						   (a2,a3) (a5,a6)  
						 then 
						   
						   prefix,(a4,Some (a1,a3,a4,a6))::same,other
						 else
						   prefix,same,(Some (a5,a6,a2,a3),(a4,None))::other)
					       
					   in 
					   match b with 
					     AL((a1,a2,a3),(a5,a6)),true -> f (a1,a2,a3,a5,a6) (prefix,same,other)
					   | L((a1,a2,a3),(a4,a5,a6)),true -> 
					     let prefix,same,other = 
					       if a1=a 
					       then f (a1,a2,a3,a5,a6) (prefix,same,other)
					       else prefix,same,other in
					     let prefix,same,other = 
					       if a4=a 
					       then f (a4,a5,a6,a2,a3) (prefix,same,other) 
					       else prefix,same,other 
					     in prefix,same,other
					   | _ -> prefix,same,other )
					 (prefix,
                                          to_visit_same_class,
					  to_visit_other_class)
					 restricted_blist in
					    aux 
					      (List.fold_left
						 (fun sol x  -> 
						   (guard,
						    bound_agent_list_same_class,
						    bound_agent_list_other_class,
						    StringSet.add a black,
						    (match from with None -> (fun x -> x)
						    | Some (a,b,c,d) -> 
						      (fun x -> add_bond_to_subspecies x (a,b) (c,d)))
						      (plug_views_in_subspecies a x prefix) ,
						    a::same_class_agent,
						    other_class)::sol) 
						 q
						 tp_list) 
					      sol
			       end
			   in
			   let rep =
			     List.map 
			       (fun x -> 
				 let list = 
				   List.filter 
				     (fun x -> not (is_empty_species x.subspecies))
				     x in
				 List.map 
				   (fun subclas -> 
				     {subclas 
				      with fragment_extension  = 
					 Some (complete_subspecies_handling_compatibility  subclas.subspecies)}) list) 
			       (aux [None,[root,None],[],StringSet.empty,empty_species,[],[]] []) in
			   {cla with subclass = Some rep}) x))
		  
		   classes in 
	       
	        let all_cases = 
		 List.map 
		   (fun classes -> 
		     (List.fold_left 
			(fun rep classe -> 
			  let subclass = 
			    match classe.subclass
			    with 
			    | None -> []
			    | Some subclass -> subclass 
			  in 
			  let l_sol = 
			    List.fold_left 
			      (fun liste case -> 
				match case
				with 
				|  [case] -> 
				  begin
				   
				    match (case.fragment_extension)
				    with 
				    |None -> liste
				    | Some case -> 
				      case@liste
				end 
				| _ -> liste)
			      [] subclass
			  in 
			  match 
			    l_sol 
			  with 
			    [] -> List.map (fun x -> (classe,None)::x) rep
			  | _ -> 
			    List.fold_left 
			      (fun liste prefix -> 
				List.fold_left 
				  (fun liste suffix -> 
				    ((classe,Some suffix)::prefix)::liste)
				  liste l_sol)
			      [] rep)
		      [[]]
		      classes))
		   classes 
		   
		in 
		
		let list_reactions = 
		 List.fold_left 
		   (fun rep  l -> 
		     List.fold_left 
		       (fun list pref -> 
			List.fold_left 
			  (fun list elt -> 
			      (elt@pref)::list)
			  list l)
		       [] rep)
		   [[]]
		   all_cases 
		in

(* reactants are computed !!!*)
	      
		let _ = 
		  List.iter 
		    (fun reactions -> 
		      let bool,consumed_list,produced_list = 
			List.fold_left 
			  (fun (bool,consumed_list,produced_list) (cla,c) -> 
			    let xx = cla.rule in 
			    let x  = cla.agents_id in 
			    let b  = get_guard_as_a_list_of_class cla in 
			    let b2 = get_guard_as_a_map_of_class cla in 
			    let filter_context_update  context_update agents = 
			      List.fold_left 
				(fun sol (b,bool) ->
				  match b,bool with 
				    H(a,a'),_ ->  sol
				  | L((a,a',b'),(c,c',d')),_ -> 
				    let sol  =
				      if StringSet.mem a agents 
				      then 
					(AL((a,a',b'),(c',d')),bool)::sol 
				      else
					sol in 
				    let sol = 
				      if StringSet.mem c agents 
				      then 
					(AL((c,c',d'),(a',b')),bool)::sol 
				      else 
					sol in
				    sol
				  | AL((a,a',b'),(c,d)),_ -> 
				    if StringSet.mem a agents 
				    then 
				      (AL((a,a',b'),(c,d)),bool)::sol 
				    else 
				      sol
				  | B((a,a',b)),_ -> 
				    if StringSet.mem a agents 
				    then 
				      (B(a,a',b),bool)::sol
				    else
				      sol 
				  | M((a,a',b),m),_ -> 
				    if StringSet.mem a agents 
				    then 
				      (M((a,a',b),m),bool)::sol
				    else sol
				  | _ -> sol) 
				[] 
				(List.rev context_update) 
			    in
			    let rep = 
			      match cla.subclass 
			      with None -> error 921
			      | Some a -> a 
			    in 
			    match b with 
			      [H (agent_id,agent),false] -> (* Agent creation *) 
				  begin 
				    let _ = dump_line 1785 in 
				    let control = 
				      filter_context_update 
					control.context_update
					(StringSet.singleton agent_id)
				    in
				    let subviews = 
				      try StringMap.find agent annotated_contact_map.subviews
				      with Not_found -> [] 
				    in 
				    let produced_list  = 
				      List.fold_left 
					(fun produced_list subview -> 
					       let blist = 
						 List.filter 
						   (fun (b,bool) -> 
						     match b with 
						       M((_,_,s),_) | B(_,_,s) | AL((_,_,s),_) -> StringSet.mem s subview.kept_sites
						     | _ -> false)
						   control  
					       in 
					       let bmap = 
						 StringSet.fold 
						   (fun site  (bmap:bool BMap.t) -> 
						     let (bmap:bool BMap.t) = 
						       List.fold_left 
							  (fun bmap m -> 
							     BMap.add (M((agent,agent,site),m)) false bmap)
							  bmap 
							  (
							    try
							      String2Map.find (agent,site) 
								(match cpb.Pb_sig.cpb_mark_site 
								 with 
								     Some a -> a 
								   | None -> error 1825)
							    with 
								Not_found -> [])
						      in 
						      let linklist = 
							try
							  String2Map.find (agent,site) 
							    (match cpb.Pb_sig.cpb_contact 
							     with 
								 Some a -> a 
							       | None -> error 1835)
							with 
							    Not_found -> [] in 
						      let (bmap:bool BMap.t) = 
							List.fold_left
							  (fun bmap l -> 
							     BMap.add (AL((agent,agent,site),l)) false bmap)
							  (BMap.add (B(agent,agent,site)) false bmap)
							  (linklist) in 
							(bmap:bool BMap.t))
						   subview.kept_sites 
						   BMap.empty 
					       in 
					       let bmap = 
						 List.fold_left 
						   (fun bmap (b,bool) -> BMap.add (downgrade_b b) bool bmap)
						   bmap blist 
					       in 
					       let blist = 
						 BMap.fold 
						   (fun b bool l -> (b,bool)::l)
						   bmap [] in 
						      
					       let blist = List.sort compare blist in 
					       let p = 
						 try 
						   (plug_views_in_subspecies 
						      agent_id 
						      (
							StringBListMap.find  
							  (agent,blist) 
							  views_data_structures.blist_to_template)
						      empty_species)
						 with 
						   Not_found -> 
						     (print_string "Invalid list of boolean attribute:\n";
						      print_string agent;
						      print_newline ();
						      List.iter 
							(fun (b,bool) -> 
							  print_string (string_of_b b);
							  print_string ":";
							     print_string (if bool then "T" else "F");
							     print_newline ())
							blist;
						      error 1828)
					       in 
					       let new_p = p,p,1,[] in 
					     	 (new_p)::produced_list)
					    produced_list subviews 
					in 
					  (true,consumed_list,produced_list)
				      end
			    | _ -> (* REGULAR STEP (not the creation of an agent *)
			      match c 
			      with Some (c:F.subspecies) -> 
				let context_update = filter_context_update control.context_update x in
				let agents = x in 
				let (context_update
				     (* we add boolean attribute modification for the current subspecies *)
					,res        (*we store the dotted bonds that are released *) 
					,solid_half (*we store the id of the agents that are removed *))
				     = 
				  begin (*A*)
				    List.fold_left 
				     (fun (context_update,res,solid_half) (a,a',b') -> 
				       let _ = 
					 if debug then 
					   (print_string a;
					    print_string a';
					    print_string b'; 
					    print_newline ()) 
				       in 
				       let b = [c] in 
				       if StringSet.mem a agents then 
					 begin
					   let _ = 
					     if debug then 
					       let _ = print_string "1592\n" in
					       let _ = 
						 List.iter 
						   (iter_views_in_species
						      (fun tp_i  -> 
							List.iter (fun (b,bool) -> 
								     print_string (string_of_b (ode_handler.b_of_var b));
							  print_string (if bool then "T\n" else  "F\n")) (bool_of_tp_i tp_i))) 
						   b 
					       in () in 
					   let optional_binding = (* scan for the type of the bond that is removed *)
					     let rec aux l = 
					       match l with [] -> None
					       | t::q -> 
						   match get_views_from_agent_id (view_of_tp_i) a a' t
						   with 
						     None -> aux q
						   | Some (view_id,view) -> 
						       let rec aux2 l = 
							 match l with 
							   (v,bool)::tail -> 
							     begin
							       match (ode_handler.b_of_var v,bool) with 
								 (AL((d,e,f),(g,h)),true) when d=a && e=a' && f=b' -> 
								   if keep_this_link (e,f) (g,h) 
								   then 
								     let target_id = d in 
(*								       try get_neighbour t (a,b') g 
								       with _ -> (print_string "ERROR";print_newline ();"") in *)
								     Some((Some target_id),g,h)
								   else
								     Some(None,g,h)
							       | _ -> aux2 tail
							     end
							 |	[] -> None
						       in aux2 (valuation_of_view view) 
					     in aux b in 
					   if optional_binding = None then 
					     (context_update,res,solid_half)
					   else 
					     let (target_id,target_type,target_site) = 
					       match optional_binding with 
						 
						 None -> error 1215
						     
					       | Some a -> a 
					     in 
					     if keep_this_link (a,b') (target_type,target_site)
					     then
					       begin
						 match target_id with 
						   None -> error 1297
						 |	Some target_id -> 
						     (*((AL((target_id,target_type,target_site),(a',b')),false))::
						     (AL((a,a',b'),(target_type,target_site)),false)::
						     (B(target_id,target_type,target_site),false)::*)
							  (B(a,a',b'),false)::
							    context_update
							    ,res,target_id::solid_half
					       end
					     else
					       ((AL((a,a',b'),(target_type,target_site)),false)::
						   context_update),
					       (target_type,target_site,a',b')::res,solid_half
					 end
				       else
					 context_update,res,solid_half)
				     (context_update,[],[])  
			      	     (control.uncontext_update)
				end (*A*)
				in 
				 let fadd ag site map = 
				     if StringSet.mem ag x 
				     then 
				       let old = 
					 try 
					   StringMap.find ag map 
					 with 
					   Not_found -> 
					     StringSet.empty 
				       in
				       StringMap.add ag (StringSet.add site old) map
				     else map 
				   in
				   let modagents =
				  (* Map agent ids -> listes of the sites that are modified by the rule *)
				     List.fold_left 
				       (fun map (b,bool) -> 
					 match b with 
					   B(a,_,s) | M((a,_,s),_) | 
					   AL((a,_,s),_) -> fadd a s map 
					 | L((a1,_,s1),(a2,_,s2)) -> 
					     fadd a2 s2 (fadd a1 s1 map)
					 | _ -> map 
					       )
				       StringMap.empty control.context_update
				   in
				   let removed_agents = 
				     List.fold_left 
				       (fun set a -> StringSet.add a set)
				       StringSet.empty 
				       control.remove
				   in 
				   let modagents = 
				     StringSet.fold 
				       (fun a map -> 
					 let sp = specie_of_id a in 
					 let site1,site2 = sites_of_agent sp in 
					 
					 let f = 
					   List.fold_left 
					     (fun map site -> fadd a site map)
					 in 
					   if site1=[] & site2 = [] 
					then (print_string a;
					   StringMap.add a StringSet.empty map)
					else f (f map site1) site2 )
				       removed_agents
				       modagents 
				   in 
				     let skeme_map = 
				  (* Map agent ids to the covering subclasses that intersect with modified sites *)
				     StringMap.mapi 
				       (fun ag site_list -> 
					 let agent_type = specie_of_id ag in
					 let skeletons = 
					   try 
					     StringMap.find agent_type annotated_contact_map.subviews 
					   with Not_found ->
					     error 1818 
					 in
					 let skeletons = 
					   List.filter 
					     (fun x -> 
						StringSet.is_empty site_list or
						  (
					       not 
						 (StringSet.is_empty
						    (StringSet.inter 
						       x.kept_sites 
						       site_list))))
					     skeletons in
					 skeletons
					   )
				       modagents 
				   in 
				       let map =
				  (* Map agent ids to the set of template piece ids that are compatible with lhs and that intersect modified sites *)
				     StringMap.mapi
				       (fun ag l ->
					 let agt = specie_of_id ag in
					 let liste = 
					   List.fold_left 
					     (fun liste int -> 
					       let int = int.kept_sites in 
					       let l = StringSet.fold (fun a b -> a::b) int [] in
					       
					       let l = List.sort compare l in 
					       let a = 
						 try 
						   StringListMap.find l 
						     (StringMap.find (specie_of_id ag) agent_to_int_to_nlist)
						 with 
						   Not_found -> 
						     error 1850 
					       in
					       let rec check_compatibility liste = 
						 match liste with 
						   [] -> true
						 | (bvar,bool)::q -> 
						     match ode_handler.b_of_var bvar with 
						       B(ag',_,s) 
						       when ag'=agt && StringSet.mem s int ->( try (bool = BMap.find (B(ag,agt,s)) b2) with Not_found -> true) && check_compatibility q
						     |	AL((ag',_,s),t) 
						       when ag'=agt && StringSet.mem s int ->( try (bool = BMap.find (AL((ag,agt,s),t)) b2) with Not_found -> true)  && check_compatibility q 
						     |	M((ag',_,s),m) 
						       when ag'=agt && StringSet.mem s int -> (try (bool = BMap.find (M((ag,agt,s),m)) b2)  with Not_found -> true) && check_compatibility q 
						     |  _ -> 
							 check_compatibility q in 
					       let filtered_a = 
						 List.filter 
						   (fun n -> 
						     check_compatibility 
						       (valuation_of_view (view_of_tp_i n)))
						   a in
					       List.fold_left 
						 (fun liste a -> 
						   a::liste) 
						 liste filtered_a)
					     
					     
					     [] 
					     
					     l in
					 liste)
				       skeme_map 
				       in
				        let map = 
				    (* Map agent ids to the set of (template piece ids/pending edges) that are compatible with lhs and that intersect modified sites *)
				     StringMap.mapi
				       (fun (agent_id:string) (a:views_id list) -> 
					 List.map 
					   (fun (i:views_id) -> 
					     let a = valuation_of_view (view_of_tp_i i) in 
					     (i,
					      List.fold_left
						(fun liste (b,bool) -> 
						  match ode_handler.b_of_var b,bool with 
						    AL((_,a,s),(a',s')),true -> 
						      if keep_this_link  (a,s) (a',s')  
						      then 
							((agent_id,a,s),(a',s'))::liste
						      else liste
						  |  _ -> liste )
						[] a))
					   a)
				       map in 
				   
					let (liste:('a*'b) list) =
				     (*list of potential (list of tp_i / pending_edges)*)
					  StringMap.fold
					    (fun ag _ sol -> 
					      let tp_list = 
						try StringMap.find ag map 
						with 
						  Not_found -> error 1436 
					      in
					      List.fold_left
						(fun ext_sol (prefix,i) ->
						  List.fold_left
						    (fun ext_sol (suffix,j) -> 
						      (StringMap.add ag prefix suffix,i@j)::ext_sol)
						    ext_sol sol)
						[]
						tp_list)
					    modagents 
					    [StringMap.empty,[]] in
					let liste = 
					  if liste = [StringMap.empty,[]]
					  then []
					  else liste 
					in 
					
					let consumed_fragment = canonical_form  c in 
					let consumed_species',free_sites = 
					  StringSet.fold 
					    (fun a cs-> 
					      remove_agent_in_species  ode_handler views_data_structures keep_this_link rule_id cs a)
					    removed_agents 
					    (c,[])
					in 
					let context_update = 
					  List.filter 
					    (fun b -> 
					      match b with 
						H(a,_),_
					      | B(a,_,_),_ 
					      | M((a,_,_),_),_ 
					      | AL((a,_,_),_),_ -> not (StringSet.mem a removed_agents) 
					      | L((a,_,_),(b,_,_)),_ -> (not (StringSet.mem a removed_agents)) && (not (StringSet.mem b removed_agents))
					      | _ -> true )
					    context_update 
					in 
					let product = 
					  apply_blist_with_species ode_handler views_data_structures keep_this_link rule_id consumed_species' context_update free_sites  in 
					(true,c::consumed_list,
					 (product,c,1,[1,c,1])::produced_list)
			      | None -> false,consumed_list,produced_list)
			      (false,[],[]) reactions 
			   in 
			  let new_binding = 
			    List.fold_left
			      (fun n_b (b,bool) -> 
				match b with 
				  L((a,a',c),(b,b',c')) 
				    when keep_this_link (a',c) (b',c') 
				      -> 
					if bool 
					then 
					  (a,a',c,b,b',c')::n_b
					else
					  n_b
				| _ -> n_b
			      )
			      [] 
			      control.Pb_sig.context_update 
			  in 
			  
			  let produced_list = 
			    fst
			      begin 
				List.fold_left 
				  (fun (product_list,hash) (species,species_to_hash,k1,k2) -> 
				    let _ = 
				      if debug 
				      then 
					print_string "BEGIN_SPLIT"
				    in
				    let connected_components = 
				      split_subspecies  views_data_structures ode_handler annotated_contact_map species in 
				    let _ = 
				      if debug 
				      then 
					print_string "END_SPLIT"
				    in
				    List.fold_left 
				      (fun (product_list,hash) a -> 
				       let (a_frag,_) = canonical_form a in
				       let key = (root_of_species a,a_frag) in 
				       let _ = 
					 if debug 
					 then 
					   let _ = print_rpath (fst (root_of_species a)) in
					   let _ = print_int (snd (root_of_species a)) in 
					   let _ = print_newline () in
					   let _ = print_string "FRAGMENT \n" in 
					   let _ = F.print_fragment   a_frag in
					   let _ = print_newline () in
					   () in 
				       let old_hash = 
					 try 
					   RootedFragmentMap.find key  hash 
				       with
					 Not_found -> 
					   empty_hash 
				       in
				       let _ = 
					 if debug 
					 then 
					 let _ = print_string "BEFORE\n" in
					 let _ = F.dump_hash old_hash in () 
				       in
				       let (old_hash',bool) = 
					 check_compatibility 
					 views_data_structures 
					   old_hash 
					   species_to_hash 
				       in
				       let _ = 
					 if debug 
					 then 
					 let _ = print_string "AFTER\n" in 
					 let _ = F.dump_hash old_hash' in 
					 let _ = F.print_species species in
					 let _ = print_string (if bool then "TRUE\n\n" else "FALSE\n\n") in 
					 () 
				       in 
				       if bool 
				       then 
					 (a,k1,k2)::product_list,
					 RootedFragmentMap.add key old_hash' hash
				     else
					 product_list,hash)
				      (product_list,hash) 
				      connected_components)
				  ([],RootedFragmentMap.empty) 
				  produced_list 
			      end
			  in 
			  let produced_list = 
			    List.fold_left 
			      (fun p_list 
				(agent_id,agent_type,site,agent_id',agent_type',site') -> 
				  let a_cand,b_cand,ab_cand,other = 
				    List.fold_left 
				      (fun 
				       (a_cand,b_cand,ab_cand,other) 
					(c,k1,k2) 
				      -> 
					match is_agent_in_species agent_id c,is_agent_in_species agent_id' c 
                                        with 
                                          true,true  -> a_cand,b_cand,(c,k1,k2)::ab_cand,other
                                        | true,false -> (c,k1,k2)::a_cand,b_cand,ab_cand,other 
                                        | _,true -> a_cand,(c,k1,k2)::b_cand,ab_cand,other
                                            | _ -> a_cand,b_cand,ab_cand,(c,k1,k2)::other
                                      )
				     ([],[],[],[]) p_list in
				   List.fold_left 
				     (fun p_list (ac,ak1,ak2) -> 
				        (List.fold_left
					   (fun p_list (bc,bk1,bk2) -> 
					      (
					        (
					          add_bond_to_subspecies 
					            (merge ac bc) 
					            (agent_id,site)
					            (agent_id',site'),
					          ak1,
					          ak2@bk2
						)::p_list))
					   p_list b_cand))
				     (List.fold_left 
                                        (fun p_list (abc,k1,k2) -> 
                                           (add_bond_to_subspecies 
                                              abc 
                                              (agent_id,site)
                                              (agent_id',site'),
                                            k1,
                                            k2)::p_list)
                                        other ab_cand) 
                                     a_cand)
			      produced_list 
			   new_binding in 

			  let print_list l = 
			    List.fold_left 
			      (fun bool c -> 
			    let _ = 
			      if bool 
			      then pprint_string print_reactions " + "
			    in 
			    let _ = pprint_int print_reactions (fst (hash_subspecies c))
			    in 
			    true)
			  false l 
		      in 
		      let _ = print_list consumed_list in 
		      let _ = pprint_string print_reactions " -> " in 
		      let _ = print_list (List.map (fun (a,_,_) -> a) produced_list) in 
		      let _ = pprint_newline print_reactions in 
		      () )
		    list_reactions 
		in 

	 (activity_map,rate_map,flag_map) 

	     end)
	(activity_map,rate_map,flag_map) 
	system in 

    let activity_map,rate_map,flag_map  = activity in 
    let _ = 
      dump 
	views_data_structures 
	ode_handler 
    in
    let lchan =  all_fields print_reactions in 
    let _ = 
      List.iter (fun chan -> 
	match chan 
	with None -> ()
	| Some chan -> 
	  List.iter 
	    close_out 
	    chan.chan)
	lchan
    in 
    Some (annotated_contact_map,activity,size ()),(l,m)
      
      
             end 
