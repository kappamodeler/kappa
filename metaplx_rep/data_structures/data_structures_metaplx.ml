(* 2009/01/08*)
(* Meta language for Kappa *)
(* Jerome Feret*)
(* Data structures *)
(* data_structures_metaplx.ml *)

open Data_structures 

type line = int

let string_of_line i = "line "^(string_of_int  i)

type agent = string 

module StringMap = Map2.Make(struct type t = string let compare = compare end)

module Agent = struct type t=agent let compare=compare end
module AgentSet = Set.Make(Agent)
module AgentMap = Map2.Make(Agent)

type site = string 
module Site = struct type t=site let compare=compare end
module SiteSet = Set.Make(Site)
module SiteMap = Map2.Make(Site)

type print_handler = 
    {string:string -> unit;
     line: unit -> unit;
     site: site -> unit;
     agent: agent -> unit}

let print_handler = 
  {string=print_string;
   line=print_newline;
   site=print_string;
   agent=print_string}

type concrete_interface = SiteSet.t

let print_interface print_string l = 
  let _ = 
    SiteSet.fold 
      (fun x bool  -> 
	(if bool then print_string.string ",");
	let _ = print_string.site x in true )
      l false 
  in () 

type action = 
    Add_site of site 
  | Delete_site of site
  | Mutate_site of site*site
  | Rename of site*(site list)

let print_action print_string act = 
  match act with 
    Add_site s -> (print_string.string "add_site ";
		   print_string.site s;
		   print_string.string ";")
  | Delete_site s -> (print_string.string "remove_site ";print_string.site s;print_string.string ";")
  | Mutate_site (s1,s2) -> (print_string.string "mutate_site ";print_string.site s1;print_string.string "-";print_string.site s2;print_string.string ";")
  | Rename (s,sl) -> 
      (print_string.string "rename_site ";
       print_string.site s;
       print_string.string "\\{";
       let _ = 
	 List.fold_left  
	 (fun bool s -> 
	   ((if bool then print_string.string ",");
	    let _ = print_string.string s in true )) 
	   false sl
	in print_string.string "\\}")


type agent_definition = 
    Root of concrete_interface 
  | Unspecified
  | Variant of agent*(action list)


let print_agent_def print_string x = 
  match x with 
    Root x -> (print_string.string "ROOT: ";
	       print_interface print_string x)
  | Variant (a,actl) -> 
      (print_string.string "VARIANT: ";print_string.agent a;print_string.string " ";List.iter (print_action print_string) actl)

type declaration = 
    { concrete_names: (concrete_interface option*line list) AgentMap.t;
      definitions: (agent_definition*line option) AgentMap.t; 
      agents: AgentSet.t } 

let print_declaration print_string x = 
  let _  = 
    AgentMap.iter
      (fun ag (a,b) -> 
	print_string.string "CONCRETE NAME: ";
	print_string.agent ag;
	print_string.string ": ";
	match a with None -> ()
	| Some a -> 
	      print_interface print_string a;print_string.line ())
      x.concrete_names 
  in
  let _ = 
    AgentMap.iter 
      (fun ag (a,b) -> 
	print_string.string "VARIANT: ";
	print_string.agent ag;
	print_string.string ": ";
	print_agent_def  print_string a;
	print_string.line ()
	)
      x.definitions 
  in 
  () 
    
type rewriting_case = 
    {target_name:agent;
     forbidden_sites:SiteSet.t; 
     substitutions:site list SiteMap.t}
     
type solved_definition = (rewriting_case list) AgentMap.t

type 'a agent_metaplx = 
    {agent_name:agent;
     interface:(site*'a) list}

 
type 'a rule_metaplx = 
    {flag:string;
     hand_side_common:('a * 'a) agent_metaplx list;
     mod_left_hand_side:'a agent_metaplx list;
     mod_right_hand_side:'a agent_metaplx list;
     fixed_left_hand_side:'a agent_metaplx list;
     fixed_right_hand_side:'a agent_metaplx list;
     sign: string;
     lhs_annotation: string;
     rhs_annotation: string;
     rule_annotation:string}

type parsed_agent = (string * (string*string) list) 
type parsed_gen =  (parsed_agent * string) option * string option * string option * action  list * string 

type parsed_conc = parsed_gen 
type parsed_rule = string * (((parsed_agent list * string) * string * (parsed_agent list * string) * string * string))


type parse = INIT_L of  (parsed_agent list *string*int)
  | DONT_CARE_L of (string*int) 
  | OBS_L of (string*string*int)
  | STORY_L of (string*string*int)
  | GEN_L of (parsed_gen*int)
  | CONC_L of (parsed_conc*int)
  | RULE_L of (parsed_rule*int) 
  | PREPROCESSED_RULE of (parsed_rule * string rule_metaplx*int)


type pp_parse = PP_INIT_L of  (((string->string) -> (parsed_agent list *string))*int)
  | PP_DONT_CARE_L of (((string->string) -> string)*int)
  | PP_OBS_L of ((string->string)->(string*string))*int
  | PP_STORY_L of ((string->string) -> (string*string))*int
  | PP_GEN_L of ((string ->string)-> parsed_gen)*int
  | PP_CONC_L of ((string->string) -> parsed_conc)*int
  | PP_RULE_L of ((string->string)->parsed_rule)*int 
  | PP_PREPROCESSED_RULE of ((string->string)->(parsed_rule * string rule_metaplx))*int
