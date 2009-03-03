(* 2009/01/08*)
(* Meta language for Kappa *)
(* Jerome Feret*)
(* Data structures *)
(* data_structures_metaplx.ml *)



type line = Rule of string | Decl 

type agent = string 



type site = string 

module SiteSet : (Set.S with type elt = site)
module SiteMap : (Map.S with type key = site)
module AgentMap : (Map.S with type key = agent)
module AgentSet : (Set.S with type elt = agent)

type print_handler = 
    {string:string -> unit;
     line: unit -> unit;
     site: site -> unit;
     agent: agent -> unit}


type concrete_interface = SiteSet.t

type action = 
    Add_site of site 
  | Delete_site of site
  | Mutate_site of site*site
  | Rename of site*(site list)


type agent_definition = 
    Root of concrete_interface 
  | Variant of agent*(action list)


type declaration =   
    { concrete_names: (concrete_interface option*line list) AgentMap.t;
      definitions: (agent_definition*line option) AgentMap.t} 


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
type parsed_gen =  (parsed_agent * string) option * string option * string option * action  list

type parsed_conc = parsed_gen
type parsed_rule = string * (((parsed_agent list * string) * string * (parsed_agent list * string) * string * string))

type parse = INIT_L of  (parsed_agent list *string)
  | DONT_CARE_L of string 
  | GEN_L of parsed_gen 
  | CONC_L of parsed_conc
  | RULE_L of parsed_rule 

val print_interface: print_handler -> concrete_interface  ->unit
val print_action: print_handler -> action -> unit
val string_of_line:line -> string
val print_handler: print_handler 
val print_declaration: print_handler -> declaration -> unit 
