(* 2009/01/08*)
(* Meta language for Kappa *)
(* Jerome Feret*)
(* Data structures *)
(* data_structures_metaplx.ml *)

open Data_structures 

type line = int 

type agent = string 
module Agent = struct type t=agent let compare=compare end
module AgentSet = Set.Make(Agent)
module AgentMap = Map2.Make(Agent)

type site = string 
module Site = struct type t=site let compare=compare end
module SiteSet = Set.Make(Site)
module SiteMap = Map2.Make(Site)

type concrete_interface=site list 

type action = 
    Add_site of site 
  | Delete_site of site
  | Mutate_site of site*site
  | Rename of site*(site list)

type agent_definition = 
    Root of concrete_interface 
  | Variant of agent*(action list)

type declaration = 
    { concrete_names: (concrete_interface option*line option) AgentMap.t;
      definitions: (agent_definition*line option) AgentMap.t} 

type rewriting_case = 
    {target_name:agent;
     forbidden_sites:StringSet.t; 
     subsitution:site list SiteMap.t}
     
type solved_definition = (rewriting_case list) AgentMap.t

    
