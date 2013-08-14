(* 15/11/2006 *)
(* Static analysis of BNG systems*)
(* Jerome Feret pour PlectiX *)
(* .dot output generation for the contact map *)
(* output_contact_map.ml *)

open Data_structures
open Pb_sig
open Tools
open Tools2 
open Neighborhood 
open Contact_map 
open Error_handler 

let error (*i*) x (*t*) y = 
    unsafe
      (Some x) 
      (Some "Complx")
      (Some "output_contact_map.ml") 
      None (*(Some t)*) 
      None (*(Some i)*) 
      y

let error_frozen (*i*) x (*t*) y = 
    unsafe_frozen
      (Some x) 
      (Some "Complx")
      (Some "output_contact_map.ml") 
      None (*(Some t)*) 
      None (*(Some i)*) 
      y

let restrict_contact_map contact binding set = 
  match set 
  with None -> contact,binding 
  | Some set -> 
      begin 
	let set = neighborhood set binding  in 
	      begin 
		List.filter (fun (a,_,_) -> StringSet.mem a set) contact,
		String2Map.fold  
		  (fun a b -> 
		    let b = List.filter (fun (a,_) -> StringSet.mem a set) b in
		    if 
		      (not (b=[])) && (StringSet.mem (fst a) set) 
		    then 
		      String2Map.add a b
		    else
		      (fun x-> x))
		  binding String2Map.empty 
	      end
	end


let dump_contact_map_in_dot interface active contact dotted fic = 
  if fic = "" or (contact = [] && interface = []  )
  then (print_string "EMPTY")
  else
    let output=open_out fic in 
    let print s = Printf.fprintf output s in 
    let _ = print "Graph G {\n" in 
    let _ =
      List.iter 
	(fun (a,b,c) ->
	   if 
	     match 
	       active
	     with 
		 None -> true 
	       | Some set -> 
		   StringSet.mem a set
	   then 
	     let m1 = list_fold StringSet.add b StringSet.empty in 
	     let m2 = list_fold StringSet.add c StringSet.empty in 
	     let mall = StringSet.union m1 m2 in 
	     let _ = print "subgraph cluster" in 
	     let _ = print "%s" a in 
	     let _ = print " {\n" in 
	     let _ = 
	       StringSet.iter 
	         (fun s -> 
		   let _ = print "%s" a in 
		   let _ = print "_" in
		   let _ = print "%s" s in 
		   let color = 
		     match StringSet.mem s m1,StringSet.mem s m2 
		     with true,false -> !Config_complx.boolean_site_color
		       | true,true  -> !Config_complx.both_site_color
		       | false,true -> !Config_complx.boundable_site_color
		       | false,false -> raise Exit in 	
		   let _ = print " [style = filled label = \"%s\"] [shape = %s] [size = %s] [fixedsize = true ] [color = %s]\n" s Config_complx.site_shape  (string_of_int Config_complx.site_size) color in 
		   ())
	         mall 
	     in 
	     let size = StringSet.cardinal  mall in 
	     let _ = print "style = filled ; label =  \"%s\"; " a in 
	     let _ = print " shape = %s;" (Config_complx.node_shape size) in 
	     let color = Config_complx.node_color size in 
	     let _ = print " color = %s };\n" color in 
	     ()
	   )
	interface
    in 
    let _ = 
      List.iter 
	(fun ((a,b),(c,d)) ->
	  print "%s" a;print "_";print "%s" b;print " -- ";print "%s" c;print "_";print "%s" d;(if dotted (a,b) (c,d) then print "[style = dotted]");print ";\n") 
	contact in
    let _ = print "};" in 		  
    let _ = close_out output in 	
    ()
      

let print_contact_map_in_dot res  pb =
  if res = High 
  then 
    begin
      let fic = !Config_complx.output_high_res_contact_dot_file in 
      let act = 
	match pb.contact_map 
	with Some l -> Some l.live_agents
	  | None -> None
      in 
      let _ = 
	match act with None -> ()
	  | Some s -> StringSet.iter print_string s 
      in 

      let l = 
	match pb.contact_map  
	with Some l -> l.relation_list  
	| None -> [] in 
      if fic = "" 
      then ()
      else
	begin	
	  let cpb = 
	    match pb.intermediate_encoding with 
	      Some cpb -> cpb 
	    | None -> error "OUTPUT_CONTACT_MAP 24 " (raise Exit)
	  in 
	  let interface = cpb.cpb_interface in
	  if l = [] && interface = [] 
	  then () 
	  else 
	    dump_contact_map_in_dot 
	      interface 
	      act 
	      l (fun _ _ -> false) fic 
	end
    end
  else
    (  
    let fic = !Config_complx.output_low_res_contact_dot_file in 
    let fic2 = !Config_complx.input_focus_on in 
    match pb.intermediate_encoding with 
      None -> ()
    | Some cpb -> 
	begin
          match cpb.cpb_contact with 
	    None -> () 
	  | Some l -> 
		if fic = "" or l = String2Map.empty 
		then () 
		else
		  begin	
		    let set = 
		      Neighborhood.good_vertice fic2  in
		    let interface,binding = 
		      restrict_contact_map 
			cpb.cpb_interface 
			l 
			set
		    in 
		    let output=open_out fic in 
		    let print s = Printf.fprintf output s in 
		    let _ = print "Graph G {\n" in 
		    let _ =
		      List.iter 
			(fun (a,b,c) ->
			  begin
			      
			    let m1 = list_fold StringSet.add b StringSet.empty in 
			    let m2 = list_fold StringSet.add c StringSet.empty in 
			    let mall = StringSet.union m1 m2 in 
			    let _ = print "subgraph cluster" in 
			    let _ = print "%s" a in 
			    let _ = print " {\n" in 
			    let _ = 
			      StringSet.iter 
				(fun s -> 
				  let _ = print "<%s" a in 
				  let _ = print "_" in
				  let _ = print "%s>" s in 
				  let color = 
				    match StringSet.mem s m1,StringSet.mem s m2 
				    with true,false -> !Config_complx.boolean_site_color
				    | true,true  -> !Config_complx.both_site_color
				    | false,true -> !Config_complx.boundable_site_color
				    | false,false -> raise Exit in 	
				  let _ = print " [style = filled label = \"%s\"] [shape = %s] [size = %s] [fixedsize = true ] [color = %s]\n" s Config_complx.site_shape  (string_of_int Config_complx.site_size) color in 
				  ())
				mall in 
			    let size = StringSet.cardinal  mall in 
			    let _ = print "style = filled ; label =  \"%s\"; " a in 
			    let _ = print " shape = %s;" (Config_complx.node_shape size) in 
			    let color = Config_complx.node_color size in 
			    let _ = print " color = %s };\n" color in 
			    () end)
			interface in 
		    let set = 
		      String2Map.fold 
			(fun (a,b) l sol -> 
			  List.fold_left  
			    (fun sol (c,d) -> 
			      begin
				  let k = 
				    if compare (a,b) (c,d) <0 
				    then (c,d),(a,b)
				    else (a,b),(c,d)
				  in
				  String4Set.add k sol end
			      )
			    sol l)
			binding String4Set.empty in
		    let _ = String4Set.iter 
		      (fun ((a,b),(c,d)) -> 
			print "<%s" a;print "_";print "%s>" b;print " -- ";print "<%s" c;print "_";print "%s>" d;print ";\n") 
			   set  
		    in
		    let _ = print "};" in 		  
		    let _ = close_out output in 	
		    ()
end end )

  	      
let dump_stoc_contact_map_in_dot interface active contact dotted subviews fic = 
  if fic = "" or (contact = [] && interface = []  )
  then (print_string "EMPTY")
  else
    let output=open_out fic in 
    let print s = Printf.fprintf output s in 
    let _ = print "Graph G {\n" in 
    let _ =
      List.fold_left
	(fun n (a,b,c) ->
	  if 
            begin
              match 
	        active
	      with 
		  None -> true 
	        | Some set -> 
		  StringSet.mem a set
            end
	  then 
            let subviews = StringMap.find a subviews in 
            let m1 = list_fold StringSet.add b StringSet.empty in 
	    let m2 = list_fold StringSet.add c StringSet.empty in 
            let size = List.length subviews in 
            List.fold_left
              (fun n mall -> 
                let a' = 
                  if size > 1 
                  then StringSet.fold (fun site s -> s^"_"^site) mall (a^"_") 
                  else a
                in 
            	let _ = print "subgraph cluster" in 
	        let _ = print "%s" (string_of_int n) in 
	        let _ = print " {\n" in 
	        let _ = 
	          List.iter 
	            (fun s -> 
		        let _ = print "<%s" a in 
		        let _ = print "_" in
		        let _ = print "%s>" s in 
		        let color = 
		          match StringSet.mem s m1,StringSet.mem s m2 
		          with true,false -> !Config_complx.boolean_site_color
		            | true,true  -> !Config_complx.both_site_color
		            | false,true -> !Config_complx.boundable_site_color
		            | false,false -> raise Exit in 	
		        let _ = print " [style = filled label = \"%s\"] [shape = %s] [size = %s] [fixedsize = true ] [color = %s]\n" s Config_complx.site_shape  (string_of_int Config_complx.site_size) color in 
		        ())
	            (List.rev (StringSet.elements mall))
	        in 
	        let size = StringSet.cardinal  mall in 
	          let _ = print "style = filled ; label =  \"%s\"; " a' in 
	          let _ = print " shape = %s;" (Config_complx.node_shape size) in 
	          let color = Config_complx.node_color size in 
	          let _ = print " color = %s };\n" color in 
	          (n+1))
              n
              subviews
          else n)
	1 interface
    in 
    let _ = 
      List.iter 
        (fun ((a,b),(c,d)) ->
          print "<%s" a;print "_";print "%s>" b;print " -- ";print "<%s" c;print "_";print "%s>" d;(if dotted (a,b) (c,d) then print "[style = dotted]");print ";\n") 
	contact in
    let _ = print "};" in 		  
    let _ = close_out output in 	
    ()
