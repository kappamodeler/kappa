(* 11/05/2007 *)
(* Static analysis of BNG systems*)
(* Jerome Feret pour PlectiX *)
(* specif d'un systeme *)
(* pb_sig *)

open Tools2  

let find_empty find x m zero = 
  try (find x m) 
  with Not_found -> zero 
      
type rule_id =
    {r_id:string;
      r_simplx:Rule.t;
      r_clone:bool}

let name_of_rule r = 
  match r.r_simplx.Rule.flag with 
    Some a -> a
  | None -> 
      ("%Auto_"^(string_of_int r.r_simplx.Rule.id)) 

let id_of_rule r = string_of_int r.r_simplx.Rule.id
let idint_of_rule r = r.r_simplx.Rule.id 

let kynetic_of_rule r = r.r_simplx.Rule.kinetics  

module RuleIdListMap = Map.Make (struct type t = rule_id list let compare=compare end)
module RuleIdSet = Set.Make (struct type t = rule_id let compare = compare end)
module RuleIdMap = Map.Make (struct type t = rule_id let compare = compare end)
 
type 'a pretty_token = Not_initialized | Abstracted | Buggy | Any  | Init of 'a
type  hflag = KNOWN | DATA | INIT | ALL 

type precision = Low | High   
type pretty = {
    is_marked:bool pretty_token;
    mark:string pretty_token;
    impossible_marks:string list pretty_token;
    is_bound:bool pretty_token;
    link:(string*string) pretty_token;
    impossible_links:(string*string) list pretty_token}
      
type pretty_fun  = {
    f_is_marked:hflag;
    f_mark:hflag;
    f_impossible_marks:hflag;
    f_is_bound:hflag;
    f_link:hflag;
    f_impossible_links:hflag}
      
      
type id_specie = string 
type name_specie = string 
type name_site = string 
type generic_site = name_specie * name_site
type site = id_specie * name_specie * name_site 
      
module IntMap = Map.Make (struct type t = int let compare = compare end)
module StringSet = Set.Make (struct type t = string let compare = compare end)
module String2Set = Set.Make (struct type t = string*string let compare = compare end)
module String22Set = Set.Make (struct type t = (string*string)*(string*string) let compare = compare end)
module String2Map = Map2.Make (struct type t = string*string let compare=compare end)
module String22Map  =Map2.Make (struct type t = (string*string)*(string*string) let compare=compare end) 
module StringMap = Map2.Make (struct type t = string let compare = compare end)
module IntSet = Set.Make (struct type t = int let compare= compare end)
    
type b = 
    Connected of (string*id_specie)*(string*id_specie)
  | H of string*name_specie
  | B of site
  | L of site * site
  | AL of site * generic_site
  | M of site * string 
  | Forb of (string*id_specie)*(string list)
  | Dis of  (string*id_specie)*(string*id_specie)
	
type key =
    Void 
  | Q of b 
  | String of string
  | String2 of string*string 
  | String_Stringlist of string * (string list)
  | Conj of key*key
  | Disj of bool*key 
  | Boo of bool 

module KeyMap=Map2.Make (struct type t = key let compare = compare end)
module KeySet=Set.Make (struct type t = key let compare = compare end)

type 'a internalsubviews = 'a KeyMap.t StringMap.t
	
let compare_b = compare
    
    
module BSet = Set.Make (struct type t = b let compare = compare_b end)
module BMap = Map2.Make (struct type t = b let compare = compare_b  end)
  
type q = QF of (string*string) | QL of (string*string*string *string) | QM of (string*string*string) | QH of string 

type quarks = 
    {test_pos: q list;
     mod_pos: q list;
     mod_neg:q list}
	
module QSet = Set.Make (struct type t = q let compare = compare end)
module QMap = Map2.Make (struct type t = q let compare = compare end)

type control = 
    {
    uncontext_update: (string*string*string) list;
    context_update: (b*bool) list;
    add: id_specie list;
    remove: id_specie list}
      
let empty_control = {
  add = [];
  context_update = [];
  uncontext_update = [];
  remove = []}
    
    
    
type 'a rule_guard = 
    {
    labels:rule_id list;
    abstract_guard: 'a StringMap.t  option;
    injective_guard: (b*bool) list;
    target: StringSet.t;
    alias: (string * string) list
  }
      
      
      
type 'a rule_class = 
    { 
    old_id: int StringMap.t;
    id: (id_specie * name_specie) list;  
    dotset:IntSet.t;
    active_species: id_specie list;
    passive_species: (site *site) list;
    abstract_lens: 'a StringMap.t option;
    rules: 'a rule_guard list;
    vars: BSet.t option;
    control: control;
    id_of_species: (string list) StringMap.t ;
    specie_of_id:  string StringMap.t;
    b_of_sites: BSet.t BMap.t ;
    b_of_binding: BSet.t BMap.t;
    b_of_id:(b list) StringMap.t;
    interface_id: (string*name_site list*name_site list) list;
    }
      
      
      
type contact_map = 
    { link_of_site: site list String2Map.t;
      possible_linksb: b list;
      relation_set: String22Set.t ;
      relation_list: ((string * string) * (string*string)) list;
      access: String2Set.t String2Map.t;
      live_agents:StringSet.t} 

type drawers = 
    { mod_agent_to_rules:IntSet.t StringMap.t;
      mod_sites_to_rules:IntSet.t String2Map.t;
      mod_edges_to_rules:IntSet.t String22Map.t;
      tested_agent_to_rules:IntSet.t StringMap.t;
      tested_sites_to_rules:IntSet.t String2Map.t;
      tested_edges_to_rules:IntSet.t String22Map.t;
 }
      
type id = int
type agent = string
type mark = string 
type cpb_site = string      
      

type cpb_state = 
    S_mark of cpb_site * mark
  | S_free of cpb_site
  | S_bound of cpb_site * (string * cpb_site)
	

type test =
    Is_here of id
  | Is_forbid of id*string list
  | Is_disjoint of id*id 
  | Is_bound of id*cpb_site
  | Is_connected of id *id 
  | Is_free of id*cpb_site
  | Is_related of (id*cpb_site)*(id*cpb_site)
  | Is_marked of (id*cpb_site)*mark

type influence_map = IntSet.t IntMap.t 
type action = 
    Bind of (id*cpb_site)*(id*cpb_site)
  | Mark of (id*cpb_site)*mark
  | Release of (id*cpb_site)*(id*cpb_site)
  | Break_half of (id*cpb_site)
  | Check_choice of (id*int) list 
  | Check_seq of (id*int)*(id*int) 
  | Check of id 
  | No_Helix
  | No_Pol
  | Rooted_story of IntSet.t

type gen_action = 
    GBind of ((string*string)*(string*string))
  | GRel_half of (string*string)
  | GMark of ((string*string)*string)
  | GRel  of ((string*string)*(string*string))
  | GCheck of string	
  | GNo_Helix
  | GNo_Pol
  | GRooted_story of StringSet.t 

module GLMap = Map.Make (struct type t = gen_action list let compare = compare end)

type cpb_control = 
    {cpb_update:action list;
     cpb_remove:IntSet.t;
     cpb_create:IntSet.t}


let cpb_empty_control = 
  {cpb_update = [];cpb_create=IntSet.empty;cpb_remove = IntSet.empty}


type cpb_rule  =
    {
    cpb_dots:IntSet.t;
    cpb_r_species:(int * agent) list;
    cpb_passive: ((int*cpb_site)*(int*cpb_site)) list option;
    cpb_guard:(((rule_id list) * IntSet.t * test list) list);
    cpb_equal: (int*int) list;
    cpb_control:cpb_control;
    cpb_quarks : quarks option 
  }
       
   
      
type 'a cpb = 
    {
    cpb_flags:string StringMap.t;
    cpb_with_dots:bool;
    cpb_species: agent list;
    cpb_contact: (string *string) list String2Map.t option; 
    cpb_sites: String2Set.t option;
    cpb_mark_site: (string list) String2Map.t option;
    cpb_interface: (agent*cpb_site list*cpb_site list) list;
    cpb_marks: mark list;
    cpb_rules: cpb_rule list;
    cpb_init: (agent * cpb_state list) list option;
    cpb_interface_of_agent: (string list * string list) StringMap.t}
  
type 'a boolean_encoding = 
    {
    system: 'a rule_class  list;
    init: ((name_specie * ((b*bool) list)) list ) option}

type options = 
    {version:string;
     date:string;
     forward:bool;
     wake_up:bool;
     inhibition:bool;
     site_abstraction:bool;
     phospho_abstraction:bool;
     ignore_linkage:bool;
     ignore_phospho:bool;
     auto_packs:bool;
     duplicate_rules_when_sym:bool;
     duplicate_rules_when_cycles:bool}
    
     
type generic_specie =
    {
      gsid:int;                         (* id *)
      name: string;                    (* name *)
      linkable_site:string list;       (* list of linkable sites *)
      phospho_site:pretty StringMap.t; (*map each markable site to some inf*)
      weigth: int
    }
      
type tagged_sites = 
    {
      specie_name: string;
      site_name: string
    }


  
module TSMap= Map2.Make (struct type t = tagged_sites let compare = compare end)

type instanciated_specie = 
    {path:string list;
     isid:int}


type complexe = 
    {card:int;
      agents:instanciated_specie list;
      available_sites: (int*(string list*int IntMap.t * int TSMap.t * string IntMap.t)*string) list;
      links:((int*(string list)*string)*(int*(string list)*string)) list;
      open_links:(int*(string list)*string) list}

type specie_map = 
    {number:int;                        (* number of puzzle species *)
     g_species:generic_specie IntMap.t;
     name_of_gsid: int -> string;
     good_binding:(int*string)->(int*string)->bool;
     which_binding:(int*string)->(int*string) list;
     specielist_of_name:string->int list;
     weigth_of_id:int -> int;
     get_sites:int -> string list}


let change_version opt1 opt2 = 
  opt1.version <> opt2.version 
  or opt1.date <> opt2.date
  or opt1.forward<>opt2.forward

let change_reachability opt1 opt2 = 
  change_version opt1 opt2 
  or  opt1.site_abstraction<>opt2.site_abstraction
  or  opt1.phospho_abstraction<>opt2.phospho_abstraction
  or  opt1.ignore_linkage<>opt2.ignore_linkage
  or  opt1.ignore_phospho<>opt2.ignore_phospho
  or  opt1.auto_packs<>opt2.auto_packs

let change_gathering opt1 opt2 = 
  change_version opt1 opt2 
  or opt1.duplicate_rules_when_sym <> opt2.duplicate_rules_when_sym
  or opt2.duplicate_rules_when_cycles <> opt2.duplicate_rules_when_cycles

let change_influence_map opt1 opt2 = 
  opt1.wake_up <> opt2.wake_up or
    opt1.inhibition <> opt2.inhibition 

type compression = (rule_id list * string list) list list 
type 'a intinf = Unbounded | Bounded of 'a 

type 'a pb = 
    {options:options option;
      nfrag:int option;
     quarks:bool;
      txt_lines: Comment_sig.commented_line  list option;
      simplx_encoding: (Rule.t list * (Solution.t*int)list * Solution.observation list * Experiment.t_unfun) option;
      first_encoding: 'a cpb option;
      intermediate_encoding: 'a cpb option;
      gathered_intermediate_encoding: 'a cpb option;

      boolean_encoding: 'a boolean_encoding option;
      gathered_boolean_encoding: 'a boolean_encoding option;
      packs: string list list StringMap.t option;
      reachability_analysis: ((string*'a) list  StringMap.t) option;    
      contact_map:contact_map option ;
      bdd_sub_views:'a internalsubviews  option;
      bdd_false:'a internalsubviews option;
      concretization:(string * (bool * pretty StringMap.t) * pretty_fun) list list option;
      reachable_complexes: (string intinf * (string list list * int)  list 
* (string list list * int) list) option;
      wake_up_map:influence_map option;
      inhibition_map:influence_map option;
      pretty_map:  pretty  StringMap.t StringMap.t;
      qualitative_compression: compression option;
      quantitative_compression: compression option;
      unreachable_rules: RuleIdSet.t option;
      rule_warning: string list RuleIdMap.t;
	specie_map:(string * string list list) list option;
      pack_value:(string * (string list * string list list) list) list option;
      n_complex:string intinf option;
      n_rules: int option ;
      n_classes:int option;
      potential_cycles: (string*string) list list option ;
      connected_components: string list list option ;
      refined_system: 'a boolean_encoding   IntMap.t ;
      drawers:drawers option ;
      refinement_relation_closure: IntSet.t IntMap.t list option;
      refinement_relation_dag: IntSet.t IntMap.t list option;
      refinement_relation_maximale: IntSet.t IntMap.t list option;
      automorphisms: int IntMap.t option
      
} 


let pb_init = 
  {
  drawers = None;
  connected_components=None;
  nfrag = None;
  n_rules = None;
  n_classes = None;
  unreachable_rules=None;
    quarks=false;
  options=None;
  txt_lines = None;
  simplx_encoding=None;
  first_encoding=None;
  intermediate_encoding=None;
  bdd_sub_views=None;
  bdd_false = None;
  gathered_intermediate_encoding=None;
  boolean_encoding=None;
  gathered_boolean_encoding=None;
  packs=None;
  reachability_analysis=None;
  contact_map=None;
  concretization=None;
  wake_up_map=None;
  inhibition_map=None;
  pretty_map=StringMap.empty;
  qualitative_compression = None;
  quantitative_compression = None;
  specie_map = None;
  pack_value = None;
  reachable_complexes = None;
  n_complex = None;
  potential_cycles = None ;
  refined_system = IntMap.empty ;
  rule_warning = RuleIdMap.empty;
  refinement_relation_closure = None;
  refinement_relation_dag = None;
  refinement_relation_maximale = None;
  automorphisms = None }


let upgrade_renaming f x = 
  match x with 
    H (a,b) -> H((f a),b)
  | B(a,b,c) -> B(f a,b,c)
  | M((a,b,c),d) -> M((f a,b,c),d)
  | AL((a,b,c),d) -> AL((f a,b,c),d)
  | L((a,b,c),(d,e,e')) -> L((f a,b,c),(f d,e,e'))
  | Connected((a,b),(c,d)) -> Connected((f a,b),(f c,d))
  | Forb((a,b),l) -> Forb ((f a,b),l)
  | Dis((a,b),(c,d)) -> Dis((f a,b),(f c,d))

let list_b_g k  =
  let upgrade (a,b,c)= (b,c) in 
  match k with
  |  H _,_  | B _,_ | AL _,_ | M _,_ -> [k]
  | L(a,b),c ->
      let a'= upgrade a in
      let b'= upgrade b in
      if c then [B(a),true;B(b),true;AL(a,b'),true;AL(b,a'),true]  else []
  | Connected _,_ | Dis _,_ | Forb _,_ -> []

let string_of_gsite (a,b) = a^"."^b	
let string_of_site (a,b,c) = a^"."^c
let order (a,b) = 
  if compare a b<0 then (a,b) else (b,a)
    
let string_of_pair_of_sites (a,b) = 
	let a = string_of_site a in 
	let b = string_of_site b in 
		if compare a b <0 then ("l("^a^","^b^")")
		else ("l("^b^","^a^")") 
let string_of_pair_of_sites_bis (a,b) = 
	let b = string_of_site b in 
	if compare a b <0 then ("l("^a^","^b^")")
	else ("l("^b^","^a^")") 

let string_of_b x = 
  let rep = match x with 
    B(a) -> (string_of_site(a)^".l")
  | M(a,b) -> "M("^(string_of_site a)^"~"^b^")"
  | H (a,b) -> "Here("^a^b^")" 
  | L(a,b) -> string_of_pair_of_sites (a,b) 
  | Connected (a,b) -> "Dot("^(string_of_gsite a)^","^(string_of_gsite b)^")"
  | AL(a,b) -> "AL("^(string_of_site a)^","^(string_of_gsite b)^")" 
  | Dis(a,c) -> "+("^(string_of_gsite a)^","^(string_of_gsite c)^")" 
  | Forb(a,l) ->  "Dis("^(string_of_gsite a)^","^(
      List.fold_left 
	(fun x a -> if x = "" then a else x^","^a)
	"" l)^")"
  in
  rep



let print_b b = print_string (string_of_b b)

let b_of_pair_of_sites x = 
  let a1,b1 = order x in L(a1,b1)
      
let l = b_of_pair_of_sites 

let topic_of_b b = 
  match b with 
     B(a,_,_) | H(a,_)  | AL((a,_,_),_) | M((a,_,_),_) ->  a
   | Connected _ | L _ | Dis _ | Forb _ -> (print_string "topic";raise Exit)

let site_of_b b = 
  match b with 
    B(a,_,b) | AL((a,_,b),_) | M((a,_,b),_) -> Some (a,b)
  | _ -> None



let dump_b channel  b  = 
   let print x = Printf.fprintf channel x in 
   let _ = print "%s" (string_of_b b) in () 

let dump_init channel x = 
   let print x = Printf.fprintf channel x in 
   let _ = print "Initial state:\n\n" in 
   let _ = 
       match x.init 
       with None -> print "Not given yet.\n" 
          | Some a -> 
              List.iter 
                 (fun (a,b) -> 
		      (print "%s: " a;
                      let _ = List.fold_left 
                            (fun bool (b,bool2) -> 
				 (if bool then print ",");
			         dump_b channel b;
				 print ":%s" (if bool2 then "true" else "false");
				 true)
		      false b in   print "\n")) a in
   let _ = print "\n" in ()

let dump_flags  channel x cpb = 
   let print x = Printf.fprintf channel x in 
   let _ = print "Rule Flags:\n\n" in 
   let _ = 
       StringMap.iter 
           (fun a b -> print "%s: %s\n" a b)
	   cpb.cpb_flags in 
   let _ = print "\n" in ()


let dump_contact  channel x contact = 
   let print x = Printf.fprintf channel x in 
   let _ = 
     (print "Contact map:\n";       
      List.iter 
        (fun ((a1,a2),(b1,b2)) -> print "%s.%s-%s.%s\n" a1 a2 b2 b1)
        contact.relation_list;
      print "\n")
   in 
   let _ = print "\n" in ()

let dump_species channel x cpb =
   let print x = Printf.fprintf channel x in 
   let _ = print "Species: " in 
   let _ = List.fold_left 
             (fun bool s -> 
                 (if bool then print ",");
                 print "%s" s;true) false
             cpb.cpb_species in 
   let _ = print "\n\n" in 
   () 

let dump_marks  channel x cpb =
   let print x = Printf.fprintf channel x in 
   let _ = print "Marks: " in 
   let _ = List.fold_left 
             (fun bool s -> 
                 (if bool then print ",");
                 print "%s" s;true) false
             cpb.cpb_marks  in 
   let _ = print "\n\n" in 
   () 

type step = Agent_id of int | Agent_gen of string | Site of string 


module PathSet = Set.Make (struct type t = step list let compare = compare end)



let dump_interface channel x cpb =
   let print x = Printf.fprintf channel x in 
   let _ = print "interface:\n\n" in 
   let _ = 
     List.iter 
        (fun (a,l1,l2) -> 
            print "Agent: %s\n" a;
            print "Markable sites: ";
            let f l = 
              List.fold_left 
                 (fun bool a -> 
                     (if bool then print ",");
                     print "%s" a;
                     true)
                 false l in 
            let _ = f l1 in 

            print "\nLinkable sites: ";
            let _ = f l2 in 
            print "\n\n")
         cpb.cpb_interface in 
   let _ = print "\n" in ()



let dump_a_rule channel  r = 
  let print x = Printf.fprintf channel x in 
  begin
    print "Rule Class: \n\n";
    print "SPECIES:\n";
    List.iter 
       (fun (i,x) -> print "%s,%s;\n" i x) 
       r.id;
    print "SPECIE->ID:\n";
    StringMap.iter 
      (fun x l -> print "%s:" x;
	let _ = List.fold_left  (fun bool x -> (if bool then print "," else ());print "%s" x;true) false l in 
	print "\n")
      r.id_of_species;
    print "ID->SPECIE:\n";
    StringMap.iter 
      (fun x1 x2 -> print "%s:%s\n" x1 x2)
      r.specie_of_id ;
    print "B_OF_SITES:\n";
    BMap.iter 
      (fun b s ->
	print "%s: " (string_of_b b);
	let _ = 
	  BSet.fold 
	    (fun b bool -> (if bool then print ",");
	      print "%s" (string_of_b b);true)
	    s false in 
	print "\n")
      r.b_of_sites;
     print "B_OF_BINDING:\n";
    BMap.iter 
      (fun b s ->
	print "%s: " (string_of_b b);
	let _ = 
	  BSet.fold 
	    (fun b bool -> (if bool then print ",");
	      print "%s" (string_of_b b);true)
	    s false in 
	print "\n")
      r.b_of_binding;
    print "\nPassive:";
    List.iter (fun ((i1,ig1,s1),(i2,ig2,s2)) -> print "(%s,%s,%s)<-(%s,%s,%s);\n" i1 ig1 s1 i2 ig2 s2) r.passive_species;
   
    print "\nGUARD:\n";
    List.iter 
      (fun r  -> 
	let a = r.labels in let b = r.injective_guard in (*TODO*)
	(print "LABEL: ";
	 List.iter (fun rid -> (print "%s"  (name_of_rule rid);
				if rid.r_clone then print " (clone)"
                                    )) a;
	 print "\n");
	(print "SUPPORT: ";
	 StringSet.iter (fun a -> print "%s, " a) r.target);
	print "\n";
	(List.iter 
	   (fun (a,b) -> print "%s:%s\n" (string_of_b a) (if b then "true" else "false")) b;print "\n";
	  print "\nAlias:";
	 List.iter (fun (i,j) -> print "%s.%s" i j) 
	   r.alias)) r.rules ;
    print "CONTEXTUAL_CONTROL:\n";
    List.iter 
          (fun (a,b) -> print "%s:%s\n" (string_of_b a) (if b then "true" else "false")) r.control.context_update;
    print "UNCONTEXTUAL_CONTROL:\n";
    List.iter 
      (fun (a,b,c) -> print "Break 1/2 line %s\n" (string_of_site (a,b,c)))
      r.control.uncontext_update;
    List.iter 
      (fun a -> print "REMOVE %s\n" a)
      r.control.remove;
    List.iter 
      (fun a -> print "ADD %s\n" a)
      r.control.add;
    match r.vars with None -> ()
    |  Some a -> 
	(print "VARIABLES:\n";
	 BSet.iter 
	   (fun b -> print "%s," (string_of_b b))
	   a;
	 print "\n")
end
    

let dump_init channel x  = 
   let print x = Printf.fprintf channel x in 
   let _ = print "Initial state:\n\n" in 
   let _ = 
       match x.init 
       with None -> print "Not given yet.\n" 
          | Some a -> 
              List.iter 
                 (fun (a,b) -> 
		      (print "%s: " a;
		       let _ = 
                      List.fold_left 
                            (fun bool (b,bool2) -> 
				 (if bool then print ",");
			         (print "%s:%s" (string_of_b b) (if bool2 then "true" else "false"));true)
		      false b in () );
		 print "\n") a in 
   let _ = print "\n" in ()

let dump_link_of_site channel x cpb = 
   let print x = Printf.fprintf channel x in 
   let _ = print "Link of sites:\n\n" in 
   let _ = 
     List.iter 
       (fun (a,b,c) -> 
	    print "AGENT %s:\n" a;
	    List.iter (fun y -> print "Site %s:\n" y;
                      List.iter (fun s -> print "%s;" (string_of_site s)) 
                                (find_empty String2Map.find (a,y) x.link_of_site []);print "\n")
            c)
       cpb.cpb_interface in 
   let _ = print "\n" in ()

let dump_interface_of_agents channel x cpb = 
   let print x = Printf.fprintf channel x in 
   let _ = print "Interface of agents:\n\n" in 
   let _ = 
     List.iter 
       (fun (a,b,c) -> 
	    print "AGENT %s:\nMarkable sites: " a;
	    let (b,c)=find_empty StringMap.find a cpb.cpb_interface_of_agent ([],[]) in 
	    List.iter (fun y -> print "%s;" y) b ;
            print "\nLinkable sites: ";
            List.iter (fun y -> print "%s;" y) c ;
            print "\n")
       cpb.cpb_interface in 
   let _ = print "\n" in ()

let dump_possible_linksb channel x contact = 
   let print x = Printf.fprintf channel x in 
   let _ = (print "Boolean for links: ";
	    List.iter (fun a -> print "%s;" (string_of_b a))
	      contact.possible_linksb;
	    print "\n") in 
   () 

let dump_possible_links channel x contact = 
   let print x = Printf.fprintf channel x in 
   let _ =  
     (print "links: ";
      List.iter (fun ((a1,s1),(a2,s2)) -> print "%s.%s-%s.%s;" a1 s1 s2 a2)
        contact.relation_list;
      print "\n") in 
   () 





let rec dump_pb fic x cpb (contact:contact_map) = 
    Tools2.log_in_file fic 
        (fun channel  -> 
           let print x = Printf.fprintf channel x in 
	   let _ = print "Boolean encoding:\n\n" in 
           let _ = dump_species channel x cpb in 
           let _ = dump_marks channel x cpb in
           let _ = begin if cpb.cpb_with_dots then print "The (BNG)-dot operator is used.\n\n" 
                                else print "The (BNG)-dot operator is not used.\n\n"
                   end in           
           let _ = dump_interface channel x cpb in 
           let _ = dump_interface_of_agents channel x cpb in  

	   let _ = dump_contact channel x contact  in
	   let _ = dump_link_of_site channel contact in 
	   let _ = dump_possible_linksb channel x contact in 
           let _ = dump_possible_links channel x  contact in 
           let _ = print "RULES:\n" in 
           let _ = List.iter (fun a -> dump_a_rule channel a) x.system in
           let _ = dump_flags channel x cpb in  
           let _ = dump_init channel x in   
                     
            ())

           
 and is_contact_map res pb  = 
	 match pb
	 with 
	   None -> false
	 | Some a -> 
	     if res = Low 
	     then 
	       match a.intermediate_encoding with None -> false
	       |  _ -> true 
	     else
	       match a.contact_map with None -> false
	       |  _ ->  true 
and is_dag_refinement_relation pb = 
  match pb 
  with 
    None -> false
  | Some a -> 
      match a.refinement_relation_dag 
      with 
	None -> false
      |	Some a -> true 
and  is_dag_refinement_relation_jpg pb file = 
  is_dag_refinement_relation pb && 
  (file <> "")
and is_maximal_refinement_relation pb = 
  match pb 
  with 
    None -> false
  | Some a -> 
      match a.refinement_relation_maximale  
      with 
	None -> false
      |	Some a -> true 
and  is_maximal_refinement_relation_jpg pb file = 
  is_maximal_refinement_relation pb && 
  (file  <> "")
	    
and is_refinement_relation_jpg pb file_dag file_max = is_dag_refinement_relation_jpg pb file_dag or is_maximal_refinement_relation_jpg pb file_max 
  
and is_intermediate_encoding pb =  
  match pb 
      with 
    None -> false
  | Some pb -> 
      match pb.intermediate_encoding 
	  with None -> false
      |	 Some a -> true
and is_gathered_intermediate_encoding  pb = 
    match pb 
      with 
    None -> false
  | Some pb -> 
      match pb.gathered_intermediate_encoding 
	  with None -> false
      |	 Some a -> true
and is_boolean_encoding pb = 
   match pb 
      with 
    None -> false
  | Some pb -> 
      match pb.boolean_encoding 
	  with None -> false
      |	 Some a -> true
and is_gathered_boolean_encoding  pb = 
   match pb 
      with 
    None -> false
  | Some pb -> 
      match pb.gathered_boolean_encoding 
	  with None -> false
      |	 Some a -> true
and is_quantitative_compression pb = 
  match pb 
      with 
    None -> false
  | Some pb -> 
      match pb.quantitative_compression
      with None -> false
      |	 Some a -> true
and is_qualitative_compression pb = 
   match pb 
      with 
    None -> false
  | Some pb -> 
      match pb.qualitative_compression
      with None -> false
      |	 Some a -> true
and is_sub_views pb = 
 match pb 
      with 
    None -> false
  | Some pb -> 
      match pb.pack_value
      with None -> false
      |	 _ -> true
and is_views pb = 
   match pb 
      with 
    None -> false
  | Some pb -> 
      match pb.bdd_sub_views 
      with None -> false
      |	 _ -> true
	  
and is_specie_map pb = 
  match pb 
  with 
    None -> false
  | Some pb -> 
      match pb.specie_map 
      with None -> false
      |	_ -> true 

and is_reachables pb =
 match pb 
      with 
    None -> false
  | Some pb -> 
      match pb.reachable_complexes,pb.n_complex
      with None,None -> false
      |	 _ -> true

let upgrade_b b id = 
  match b with 
    H(_,a) -> H(id,a)
  | B(_,a,b) -> B(id,a,b)
  | M((_,a,b),c) -> M((id,a,b),c)
  | AL((_,a,b),c) -> AL((id,a,b),c)
  | _ -> b

let downgrade_b b = 
  match b with 
    H(_,a) -> H(a,a)
  | B(_,a,b) -> B(a,a,b)
  | M((_,a,b),c) -> M((a,a,b),c)
  | AL((_,a,b),c) -> AL((a,a,b),c)
  | _ -> b

