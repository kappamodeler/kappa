(* 17/02/2009 *)
(* Meta language for Kappa systems *)
(* Jerome Feret*)

open Data_structures_metaplx 
open Rename_agent 
let x = 
  {
  concrete_names = 
  AgentMap.add "A1" (Some ["s"],[]) 
    (AgentMap.add "A2" (Some ["s1";"s2";"y"],[]) AgentMap.empty);
  definitions = 
  AgentMap.add 
    "A2" 
    (Variant ("A3",[Rename("s",["s1";"s2"])]),None)
    (AgentMap.add "A3" (Root ["s";"y"],None) 
       (AgentMap.add 
	  "A4"
	  (Variant ("A2",[Rename("s1",[])]),None)
	  AgentMap.empty
      )
   )
} 
    
let subs = Agent_tree.convert_declaration_into_solved_definition x
let x = 
  Rename_agent.rename_agent 
    {agent_name = "A3" ;
      interface = ["s",();"y",()]}
    subs 
let y = Rename_agent.rename_agent 
    {agent_name = "A3" ;
     interface = ["y",()]}
    subs 
let z = Rename_agent.rename_agent 
    {agent_name = "A2" ;
      interface = ["s1",()]}
    subs 

let _ = print_string "X:\n" 
let _ = List.iter print_agent x
let _ = print_newline ()

let _ = print_string "Y:\n"
let _ = List.iter print_agent y
let _ = print_newline ()

let _ = print_string "Z:\n"
let _ = List.iter print_agent z 
let _ = print_newline () 


