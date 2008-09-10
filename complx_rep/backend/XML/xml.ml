(* 11/05/2007 *)
(* Static analysis of BNG systems*)
(* Jerome Feret pour PlectiX *)
(* output for XML *)
(* xml.ml *)

open Data_structures
open Pb_sig
open Tools
open Config_complx 
open Output_contact_map 
open Error_handler 

let style () = 
  "xsi:schemaLocation=\"http://plectix.synthesisstudios.com
KappaSession.xsd\" xmlns=\"http://plectix.synthesisstudios.com/schemas/kappasession\"
xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
  
let time_stamp () = Config_complx.time_stamp 
let command_line () =  
  let s = ref "" in 
  let _ = 
    Array.iter  (fun x  -> if (!s) = "" then (s:=x) else s:=((!s)^" "^x)) Sys.argv in (!s)

let input_file () = List.fold_right (fun x s -> if s="" then x else s^" "^x) (!Config_complx.input_file) ""

let print_log channel (l,m) list   =
  let print s = Printf.fprintf channel s in 
    if m = [] && l = [] then ()
    else 
      let _ = print "<Log>\n" in 
      let _ = List.iter (fun s -> print "<Entry Type=\"WARNING\" Message=\"%s\"/>\n" s) m in 
      let _ = 
	match List.rev l with [] -> ()
	  | (s,t)::q  ->
	      let _ = list_fold
		(fun (s,t) t' -> print "<Entry Type=\"INFO\" Message=\"%s%s s.\"/>\n" s (string_of_float (t-.t'));t)
		q t in () in 
      let _ = dump_error_in_XML channel list in 
	
      let _ = print "</Log>\n" in 
	() 

let print_pack_value channel pb = 
  match pb.pack_value with 
    None -> ()
  | Some a -> 
      let print s = Printf.fprintf channel s in
      let _ = print "<Reachables Name=\"Subviews\">\n" in 
      let _ = 
	List.iter 
	  (fun (a,b) -> 
	    List.iter 
	      (fun (c,d) -> 
		let _ = print "<Set Agent=\"%s\">\n" a in 
		let _ = List.iter (fun x -> print "<Tag Data=\"%s\" />\n" x) c in
		let _ = List.iter (fun x -> 
		  let _ = print "<Entry Data=\"" in
		  let _ = List.iter (fun x -> print "%s" x) x in
		  let _ = print "\"/>\n" in ())
		  d in
		let _ = print "</Set>\n" in 
		())
	      b)
	  a in
      let _ = print "</Reachables>\n" in 
      ()

let print_specie_map channel pb = 
  match pb.specie_map with 
    None -> ()
  | Some a -> 
      let print s = Printf.fprintf channel s in
      let _ = print "<Reachables Name=\"Views\">\n" in 
      let _ = 
	List.iter 
	  (fun (a,b) -> 
	    let _ = print "<Set Agent=\"%s\">\n" a in 
	    let _ = 
	      List.iter 
		(fun x -> 
		  let _ = print "<Entry Data=\"" in
		  let _ = List.iter (fun x -> print "%s" x) x in
		  let _ = print "\"/>\n" in ())
		b in 
	    let _ = print "</Set>\n" in 
	    ())
  	  a in
      let _ = print "</Reachables>\n" in 
      ()

let print_enumeration channel pb = 
  match pb.reachable_complexes with 
    None -> ()
  | Some (a,b,c) -> 
      let print s = Printf.fprintf channel s in
      let _ = print "<Reachables Name=\"Species\" Cardinal=\"" in
      let _ = match a with Bounded n -> print "%s" n
      |	Unbounded -> print "Unbounded" in
      let _ = print "\">\n" in 
      let _ = 
	match b,c with [],[] -> ()
	| [_,0],[] -> ()
	|  _ -> 
	    let _ = print "<Set Name=\"All Species\">" in
	    let _ = 
	      List.iter 
	  (fun (x,n) -> 
	    let _ = print "<Entry Type=\"Close\" Weight=\"%d\" Data=\"" n in
	    let _ = List.iter 
		(List.iter (print "%s")) x in
	    let _ = print "\"/>\n" in ())
		b in
	    let _ = 
	      List.iter 
		(fun (x,_) -> 
		  let _ = print "<Entry Type=\"Open\" Weight=\"Unbounded\" Data=\"" in
		  let _ = 
		    List.iter 
		      (List.iter (print "%s")) 
		      x in
		  let _ = print "\"/>\n" in ())
		c in
	    let _ = print "</Set>\n" in ()
      in
      let _ = print "</Reachables>\n" in 
      ()

let print_compression mode channel pb = 
  let a,title = 
    if mode = Full then pb.qualitative_compression,"Qualitative compression"
    else pb.quantitative_compression,"Quantitative compression" in 
  match a  with 
    None -> () 
  | Some a -> 
      let print s = Printf.fprintf channel s in 
      let _ = print "<RuleSet Name=\"%s\">\n" title in 
      let n = ref 0 in 
      let fresh () = 
	(n:=(!n)+1;
	!n) in 
      let map = ref IntMap.empty in 
      let asso k n = 
	let old = 
	  try (IntMap.find n (!map))
	      with Not_found -> [] in 
	map:=IntMap.add n (k::old) (!map)
      in
      let _  =
	List.iter
	  (fun l -> 
	    List.iter 
	      (fun (a,b) -> 
		let n = fresh () in 
		let _ = print "<Rule Id=\"%d\" Name=\"" n in
		let _ = 
		    List.fold_left 
		    (fun bool s -> 
		      (asso s n;
if bool then print ",%s" (name_of_rule s)
		      else print "%s" (name_of_rule s));
		      true)
		    false (List.sort compare a) in
		let _ = print "\" Data=\"" in 
		let _ = 
		  List.iter 
		    (fun s -> print "%s" s)
		    (List.rev b) in
		let _ = print "\"/>\n" in ())
		l)
	      (List.rev a) in
      let _ = print "<Map FromSet=\"Original\">\n" in 
      let g id =  id.Pb_sig.r_simplx.Rule.id in 
	
      let _ = 
	IntMap.iter 
	  (fun a b -> 
	    List.iter 
	      (fun c -> 
		print "<Association FromRule=\"%d\" ToRule=\"%d\"/>\n"
		  (g c) a)
	      b)
	  (!map) in 
      let _ = print "</Map>\n" in 
      let _ = print "</RuleSet>\n" in
      ()

let print_rules channel pb  = 
  try (let system = 
    match pb.boolean_encoding with 
      Some a -> a.system
    | None -> 
	(match pb.gathered_boolean_encoding with 
	  None -> raise Exit
	| Some a -> a.system) in
  let print s = Printf.fprintf channel s in 
  let _ = print "<RuleSet Name=\"Original\">\n" in 
  let _ = 
    List.iter 
      (fun rc -> 
	List.iter 
	  (fun case -> 
	    List.iter 
	      (fun id -> 
		if not (id.Pb_sig.r_clone) then 
		  print "<Rule Id=\"%d\" Name=\"%s\" Data=\"%s\"/>\n" 
			      id.Pb_sig.r_simplx.Rule.id
		    (name_of_rule id)
		    id.Pb_sig.r_simplx.Rule.input)
	      case.Pb_sig.labels)
	  rc.Pb_sig.rules)
      (List.rev system) in
	    let _ = print "</RuleSet>\n" in
	    let _ = print_compression Full channel pb in
	    let _ = print_compression Isolated channel pb in 
	    ())
  with _ -> ()
      
let print_contact_map channel pb   = 
  try (let cpb =
    match pb.intermediate_encoding with 
      None -> raise Exit
    | Some a -> a in 
  let print s = Printf.fprintf channel s in 
  let print_rule r = print "<Rule Id=\"%i\"/>\n" r in
  let iter_ag ag f = 
	match pb.drawers with 
	  None -> ()
	| Some a -> 
	    let rules = 
	      try 
		StringMap.find ag a.agent_to_rules 
	      with 
		Not_found -> IntSet.empty 
	    in
	    IntSet.iter f rules  in
  let iter_sites site f = 
    match pb.drawers with 
      None -> ()
    | Some a -> 
	let rules = 
	  try 
	    String2Map.find site a.sites_to_rules
	  with 
	    Not_found -> IntSet.empty 
	in
	IntSet.iter f rules  in
   let iter_edges edge f = 
     let (a,b) = edge in 
     let edge = 
       if compare a b < 0 then edge else (b,a) in 
    match pb.drawers with 
      None -> ()
    | Some a -> 
	let rules = 
	  try 
	    String22Map.find edge a.edges_to_rules
	  with 
	    Not_found -> IntSet.empty 
	in
	IntSet.iter f rules  in
   let _ = 
     begin
       match cpb.cpb_contact 
       with None -> () 
       |	Some rep -> 
	   let _ = print "<ContactMap Name=\"Low resolution\">\n" in 
	  let fic2 = !Config_complx.input_focus_on in 
	  let set = Neighborhood.good_vertice fic2  in
	  let interface,binding = 
	    restrict_contact_map 
	      cpb.Pb_sig.cpb_interface 
	      rep 
	      set in 
	  let _ = 
	    List.iter 
	      (fun (a,b,c) ->
		let m1 = list_fold StringSet.add b StringSet.empty in 
		let m2 = list_fold StringSet.add c StringSet.empty in 
		let mall = StringSet.union m1 m2 in 
		let _ = print "<Agent Name=\"%s\">\n" a in 
		let _ = iter_ag a print_rule in 
		let _ = 
		  StringSet.iter 
		    (fun site -> 
		      let _ = print 
			  "<Site Name=\"%s\" CanChangeState=\"%s\" CanBeBound=\"%s\">\n" 
			  site 
			  (if StringSet.mem site m1 then "true" else "false")
			  (if StringSet.mem site m2 then "true" else "false")
		      in 
		      let _ = iter_sites (a,site)  print_rule in 
		      let _ = print "</Site>\n" in () )
		    mall in 
		let _ = print "</Agent>\n" in ()) 
	      interface in 
	  let set = 
	    String2Map.fold 
	      (fun (a,b) l sol -> 
		List.fold_left  
		  (fun sol (c,d) -> 
		    let k = 
		      if compare (a,b) (c,d) <0 
		      then (c,d),(a,b)
		      else (a,b),(c,d)
		    in
		    String4Set.add k sol)
		  sol l)
	      binding String4Set.empty in
	  let _ = String4Set.iter 
	      (fun ((a,b),(c,d)) -> let _ = 
	  	print "<Bond FromAgent=\"%s\" FromSite=\"%s\" ToAgent=\"%s\" ToSite=\"%s\">\n" a b c d in
	      let _ = iter_edges ((a,b),(c,d)) print_rule in 
	      let _ = print "</Bond>" in ()
		  ) set in 
  	  let _ = print "</ContactMap>\n" in ()
    end in  
  let _ = 
    match pb.contact_map  
    with None -> ()
    | Some l -> 
	let l = l.relation_list in 
	begin
	  let print s = Printf.fprintf channel s in 
	  let _ = print "<ContactMap Name=\"High resolution\">\n" in 
	  let _ = 
	    List.iter 
	      (fun (a,b,c) ->
		let m1 = list_fold StringSet.add b StringSet.empty in 
		let m2 = list_fold StringSet.add c StringSet.empty in 
		let mall = StringSet.union m1 m2 in 
		let _ = print "<Agent Name=\"%s\">\n" a in
		let _ = iter_ag a print_rule in 
		let _ = 
		  StringSet.iter 
		    (fun site -> 
		      let _ = print 
			  "<Site Name=\"%s\" CanChangeState=\"%s\" CanBeBound=\"%s\">\n" 
			  site 
			   (if StringSet.mem site m1 then "true" else "false")
			  (if StringSet.mem site m2 then "true" else "false")
		      in 
		      let _ = iter_sites (a,site)  print_rule in 
		      let _ = print "</Site>\n" in 
		      ())
		    mall in 
		let _ = print "</Agent>\n" in ()) 
	      cpb.Pb_sig.cpb_interface in 
	  let _ = 
	    List.iter (fun ((a,b),(c,d)) ->
	      let _ = print "<Bond FromAgent=\"%s\" FromSite=\"%s\" ToAgent=\"%s\" ToSite=\"%s\">\n" a b c d in
	        let _ = iter_edges ((a,b),(c,d)) print_rule in
		let _ = print "</Bond>" in ()
		      ) l
	  in 
	  let _ = print "</ContactMap>\n" in ()
	end in 
  	()  )
      with _ -> ()


let print_flow_map channel pb  = 
  try (let system = 
    match pb.boolean_encoding with 
      None -> 
	(match pb.gathered_boolean_encoding with 
	  None -> (raise Exit)
	| Some a -> a.system )
    | Some a -> a.system in 
  let print s = Printf.fprintf channel s in 
  match pb.Pb_sig.wake_up_map,pb.Pb_sig.inhibition_map with 
      Some a,Some b -> 
	let _ = print "<InfluenceMap Name=\"Low resolution\">\n"  in 
	let _ = 
	  List.iter 
	    (fun rc -> 
	       List.iter 
		 (fun case -> 
		    List.iter 
		      (fun id -> 
			if not id.Pb_sig.r_clone then 
			let _ = (match id.Pb_sig.r_simplx.Rule.flag with 
				None -> id.Pb_sig.r_simplx.Rule.input
				  | Some a -> a) in 
				 
			 let _  =
			   print "<Node Type=\"RULE\" Text=\"%s\" Id=\"%d\" Name=\"%s\" Data=\"%s\"/>\n"  
			     (name_of_rule id)
			     (id.Pb_sig.r_simplx.Rule.id)
			     (name_of_rule id)
			     id.Pb_sig.r_simplx.Rule.input  in ())
		      (List.rev case.Pb_sig.labels))
		 (List.rev rc.Pb_sig.rules))
	    (List.rev system) in
	let _ = 
	  IntMap.iter 
	    (fun a b -> 
	       IntSet.iter 
		 (fun b -> 
		    let _ = print "<Connection FromNode=\"%d\" ToNode=\"%d\" Relation=\"POSITIVE\"/>\n" a b in ()) b)
	    a in
	let _ = 
	  IntMap.iter 
	    (fun a b -> 
	       IntSet.iter 
		 (fun b -> 
		    let _ = print "<Connection FromNode=\"%d\" ToNode=\"%d\" Relation=\"NEGATIVE\"/>\n" a b in ()) b)
	    b in
	let _ = print "</InfluenceMap>\n" in ()  
  |  _ -> ()) with _ -> ()

let print_refinement_relation channel pb = 
  let _ = 
    begin
      match pb.simplx_encoding 
      with 
	None -> ()  
      | Some (rules,_) -> 
	  let _ = 
	    match pb.refinement_relation_dag with 
	      None -> ()
	    | Some a -> Refinements.dump_refinement_in_XML "DAG" channel rules a 
	  in
	  let _ = 
	    match pb.refinement_relation_maximale  with 
	      None -> ()
	    | Some a -> Refinements.dump_refinement_in_XML "Maximal" channel rules a 
	  in
	  () 
    end
  in
  () 

let print_automorphisms channel pb = 
  let _ = 
     begin
      match pb.simplx_encoding 
      with 
	None -> ()  
      | Some (rules,_) -> 
	  let _ = 
	    match pb.automorphisms with 
	      None -> ()
	    | Some a -> Count_isomorphism.dump_automorphisms_in_XML  channel rules a 
	  in
	  () 
    end
  in
  () 
	
let dump_session pb channel (l,m)  = 
  let print s = Printf.fprintf channel s in 
  let _ = print "<?xml version='1.0' encoding='utf-8'?>\n" in 
  let _ = print "<!-- Automaticaly generated by Complx %s -->\n" (Config_complx.version) in
  let _ = print "<ComplxSession Timestamp=\"%s\" CommandLine=\"%s\" InputFile=\"%s\" \n"
    (time_stamp ()) 
    (command_line ()) 
    (input_file ()) in 
  let _ = print "%s>\n" (style ()) in 
  let _ = print_log channel (l,m) (!error_list)  in 
  let _ = match pb with None -> ()
    | Some pb -> 
	let _ = print_rules channel pb  in 
	let _ = print_contact_map channel pb  in 
	let _ = print_flow_map channel pb  in 
	let _ = print_pack_value channel pb  in 
	let _ = print_specie_map channel pb in 
	let _ = print_enumeration channel pb in
	let _ = print_refinement_relation channel pb in 
	let _ = print_automorphisms channel pb in 
	() in 
  let _ = print "</ComplxSession>\n" in 
    (l,m)
