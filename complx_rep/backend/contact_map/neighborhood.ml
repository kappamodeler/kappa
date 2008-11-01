open Kappa_parse
open Data_structures
open Pb_sig 
open Quarkification

let good_vertice file  = 
 	 match file with "" -> None
	 |  a -> 
	     let tmp_forward = !Data.forward in
	     let tmp_mode = !Data.compile_mode in
	     let _ = Data.forward:= (!Config_complx.forward) in 
	     let _ = Data.compile_mode:=true in 
	     let (a,b,_,_) = Kappa_lex.compile a  in 
	     let b = !Data.init in (*modif JK*)
	     let _ = Data.forward:= tmp_forward in
	     let _ = Data.compile_mode := tmp_mode in 

	     let cpb,messages = Translate.translate_rule_list (List.rev a) b None [] in 
	     match cpb.cpb_contact 
	     with None -> None
	     | Some pre_contact ->
		 let contact = 
		   String2Map.fold 
		     (fun s m sol -> 
		       List.fold_left 
			 (fun sol s2 -> 
			   Contact_map.add_contact cpb.cpb_with_dots s s2 sol)
			 sol m)
		     pre_contact  
		     Contact_map.contact_map_init in 
		 let rules =  List.map (quarkify cpb contact) cpb.cpb_rules in
		 let enrich l set = 
		   List.fold_left 
		     (fun set quark -> 
		       match quark with 
			 QF (a,_) | QH(a) | QM(a,_,_) -> StringSet.add a set
		       | QL(a,_,_,b) -> StringSet.add a (StringSet.add b set))
		     set l 
		 in
		 Some (List.fold_left
		   (fun set rule -> 
		     match rule.cpb_quarks with None -> set
		 | Some q ->
		     enrich q.mod_pos (enrich q.mod_neg set))
	       StringSet.empty rules )


(*
	     Some 
	       (List.fold_left 
		  (fun set rule -> 
		    Mods.IntMap.fold 
		      (fun _ agent -> StringSet.add agent.Agent.name)
		      rule.Rule.add
		      (Mods.IntMap.fold 
			 ( fun _ rule set ->
			   List.fold_left 
			     (fun set (a,b) ->
			       match a with 
				   Bind of (string*int*string) 
		| Break of (string*int*string)
		| Mark of (string*string)
		| Modify of string (*break a semi-link*)
		| Remove _ -> set)
			     set rule)
			 rule.Rule.actions set)
		      )
		  StringSet.empty a) *)
		    
let neighborhood set contact_map = 
  String2Map.fold 
    (fun (a,b) l res ->
      List.fold_left 
	(fun res (c,d) ->
      match  StringSet.mem a set,
	     StringSet.mem c set 
      with 
	true,true -> res
      |	true,false -> StringSet.add c res
      |	false,true -> StringSet.add a res
      |	_ -> res)
	res l)
	contact_map set
