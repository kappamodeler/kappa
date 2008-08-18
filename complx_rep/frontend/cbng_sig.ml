(* 22/02/2007 *)
(* Static analysis of BNG systems*)
(* Jerome Feret pour PlectiX *)
(* Intermediar code *)
(* cbng *)
open Rule 
open Pb_sig
open Data_structures
open Tools 


let trace = false


module GLMap = Map.Make (struct type t = gen_action list let compare = compare end)



let dump_init channel x = 
   let print x = Printf.fprintf channel x in 
   let _ = print "Initial state:\n\n" in 
   let _ = 
       match x.cpb_init 
       with None -> print "Not given yet.\n" 
          | Some a -> 
              List.iter 
                 (fun (a,b) -> 
		      (let _  = print "%s: " a in 
                       let _  = List.fold_left 
                            (fun bool state -> 
				 (if bool then print ",");
			         (match state 
				  with S_mark (s,m) -> print "%s~%s" s m
				  | S_free s -> print "%s" s 
                                  | S_bound (s,(a,s2)) -> print "%s!%s.%s" s a s2);
				 true)
		      false b in ());
		 print "\n") 
		a in 
   let _ = print "\n" in ()

let dump_flags  channel x = 
   let print x = Printf.fprintf channel x in 
   let _ = print "Rule Flags:\n\n" in 
   let _ = 
       Pb_sig.StringMap.iter 
           (fun a b -> print "%s: %s\n" a b)
	   x.cpb_flags in 
   let _ = print "\n" in ()


let dump_contact  channel x = 
   let print x = Printf.fprintf channel x in 
   let _ = 
   match x.cpb_contact with 
    None -> print "The contact map is not computed yet.\n"
    | Some a -> 
         print "Contact map:\n";       
         (String2Map.iter 
            (fun (a1,a2) b ->
                print "%s.%s:" a1 a2;
                let _ = 
		  list_fold
                    (fun (a1,a2) bool -> 
                       (if bool then print ",");
                       print "%s.%s" a1 a2;
                       true)
                    b false in print "\n")
            a;print "\n") in 
   let _ = print "\n" in ()


let dump_mark_site channel x = 
  let print x = Printf.fprintf channel x in 
   let _ = 
   match x.cpb_mark_site with 
    None -> print "Marks/sites relation is not computed yet.\n"
    | Some a -> 
         print "Marks/Sites Relation:\n";
         (String2Map.iter 
            (fun (a1,a2) b ->
                print "%s.%s:" a1 a2;
                let _ = 
		  list_fold
                     (fun m bool -> 
                          (if bool then print ",");
                          print "%s" m;
                          true)
                     b false in print "\n")
            a;print "\n") in 
   let _ = print "\n" in ()
  
let dump_species channel x =
   let print x = Printf.fprintf channel x in 
   let _ = print "Species: " in 
   let _ = List.fold_left 
             (fun bool s -> 
                 (if bool then print ",");
                 print "%s" s;true) false
             x.cpb_species in 
   let _ = print "\n\n" in 
   () 

let dump_marks  channel x =
   let print x = Printf.fprintf channel x in 
   let _ = print "Marks: " in 
   let _ = List.fold_left 
             (fun bool s -> 
                 (if bool then print ",");
                 print "%s" s;true) false
             x.cpb_marks  in 
   let _ = print "\n\n" in 
   () 



let dump_interface channel x =
   let print x = Printf.fprintf channel x in 
   let _ = print "interface:\n\n" in 
   let _ = 
     List.iter 
        (fun (a,l1,l2) -> 
            print "Agent: %s\n" a;
            print "Markable sites: ";
            let f l = 
              let _ = List.fold_left 
                 (fun bool a -> 
                     (if bool then print ",");
                     print "%s" a;
                     true)
                 false l in () in
            f l1;

            print "\nLinkable sites: ";
            f l2;
            print "\n\n")
         x.cpb_interface in 
   let _ = print "\n" in ()

let print_quark a = 
  match a with 
  | QF(a,s) -> "QF"^a^s
  | QL(a,s,a',s') -> "QL"^a^s^a'^s'
  | QM(a,s,m) -> "QM"^a^s^m
  | QH(a) -> "Q"^a
			 
let dump_a_rule channel  r = 
  let print x = Printf.fprintf channel x in 
  begin
    print "Rule: \n\n ";
    print "SPECIES: ";
    List.iter 
       (fun (i,x) -> print "%d,%s;" i x) 
       r.cpb_r_species;
    (match r.cpb_quarks with 
      None -> ()
    | Some q -> 
	print "\nQUARKS \n";
	print "\nTEST: ";
	List.iter (fun a -> print "%s;" (print_quark a)) q.test_pos;
        print "\n MOD+ : ";
	List.iter (fun a -> print "%s;" (print_quark a)) q.mod_pos;
	print "\n MOD- : ";
	List.iter (fun a -> print "%s;" (print_quark a)) q.mod_neg)
      ;
    (match r.cpb_passive with 
      None -> ()
    | Some a -> print "\nPASSIVE \n";
	        List.iter (fun ((i1,s1),(i2,s2)) -> print "%d,%s,%d,%s;" i1 s1 i2 s2) a);
    (match r.cpb_equal with 
      a ->   (print "\nEQUAL: \n ";
	      List.iter 
		(fun (x,y) -> print "%d,%d" x y) a));
    print "\nGUARD \n";
    List.iter 
      (fun (a,c,b) ->
	(print "LABEL: ";
	 List.iter (fun a -> print "%s"  (name_of_rule a)) a;
	 print "\n");
	print  "SUPPORT: ";
	IntSet.iter (fun a -> print "%d" a) c;
	print "\n";
	
	(List.iter (fun x -> 
	  match x with Is_here x -> print "Here %d;\n" x
	  | Is_marked((x,a),m) -> print "Mark %d.%s:%s;\n" x a m
	  | Is_bound (x,a) -> print "Bound %d.%s;\n" x a 
	  | Is_free(x,a) -> print "Free %d.%s;\n" x a
          | Is_related((x,a),(y,b)) -> print "Related %d.%s,%d.%s;\n" x a y b 
          | Is_connected _ -> () 
	  | Is_disjoint(a,c) -> print "Type_disjoint %d.%d" a c 
	  | Is_forbid (a,l) ->  (print "Black list %d." a ;
				 List.iter (fun s -> print "%s," s) l;
				 print "\n"))
	 b);
      print "\n")
      r.cpb_guard;
    print "CONTROL \n";
    List.iter (fun x -> 
      match x with 
	Mark((x,s),m) -> print "MARK %d,%s,%s;\n" x s m
      | Check_choice l -> (print "CHECK_CHOICE:";
                          let _ = List.fold_left 
			     (fun bool a -> 
				  ((if bool then print ",");
                                   print "%d" a;
                                   true))
                                  false l in 
                           print "\n")
                                  
      |	Check x -> print "CHECK %d;\n" x
      | Release((x,sx),(y,sy)) -> print "RELEASE %d,%s,%d,%s;\n" x sx y sy 
      | Break_half  (x,sx) -> print "Release 1/2 %d,%s" x sx
      | Bind((x,sx),(y,sy)) ->  print "BIND %d,%s,%d,%s;\n" x sx y sy 
       )
      r.cpb_control.cpb_update;
    IntSet.iter 
      (fun i -> print "REMOVE %d\n" i)
      r.cpb_control.cpb_remove;
    IntSet.iter
      (fun i -> print "ADD %d \n" i)
      r.cpb_control.cpb_create
end
    

let dump_init channel x = 
   let print x = Printf.fprintf channel x in 
   let _ = print "Initial state:\n\n" in 
   let _ = 
       match x.cpb_init 
       with None -> print "Not given yet.\n" 
          | Some a -> 
              List.iter 
                 (fun (a,b) -> 
		      (print "%s: " a;
                      let _ = List.fold_left 
                            (fun bool state -> 
				 (if bool then print ",");
			         (match state 
				  with S_mark (s,m) -> print "%s~%s" s m
				  | S_free s -> print "%s" s 
                                  | S_bound (s,(a,s2)) -> print "%s!%s.%s" s a s2);
				 true)
		      false b in ());
		 print "\n") a in 
   let _ = print "\n" in ()

let cbng_dump fic x = 
    Tools2.log_in_file fic 
        (fun channel  -> 
           let print x = Printf.fprintf channel x in 
           let _ = print "Cbng encoding:\n\n" in 
           let _ = dump_species channel x in 
           let _ = dump_marks channel x in
           let _ = begin if x.cpb_with_dots then print "The (BNG)-dot operator is used.\n\n" 
                                else print "The (BNG)-dot operaor is not used.\n\n"
                   end in           
           let _ = dump_interface channel x in 
           let _ = dump_contact channel x in
	   let _ = dump_mark_site channel x in 
           let _ = print "RULES:\n" in 
           let _ = List.iter (fun a -> dump_a_rule channel a) x.cpb_rules in
           let _ = dump_flags channel x in  
       let _ = dump_init channel x in      
           
            ())

           
