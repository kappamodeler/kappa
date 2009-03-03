(* 17/02/2009 *)
(* Meta language for Kappa systems *)
(* Jerome Feret*)

open Data_structures_metaplx 
open Rename_agent 
open Lexing 
open Meta_lex

let compile fic =
    let d = open_in fic in
    let lexbuf = Lexing.from_channel d in
    let rep = Meta_parse.main token lexbuf in 
    rep 
    

let x = 
  {
  concrete_names = 
  AgentMap.add "A1" (Some (SiteSet.add "s" SiteSet.empty),[]) 
    (AgentMap.add "A2" (Some (SiteSet.add "s1" (SiteSet.add "s2" (SiteSet.add "y" SiteSet.empty))),[]) AgentMap.empty);
  definitions = 
  AgentMap.add 
    "A2" 
    (Variant ("A3",[Rename("s",["s1";"s2"])]),None)
    (AgentMap.add "A3" (Root (SiteSet.add "s" (SiteSet.add "y" SiteSet.empty)),None) 
       (AgentMap.add 
	  "A4"
	  (Variant ("A2",[Rename("s1",[])]),None)
	  (AgentMap.add 
	     "A5" 
	     (Variant ("A2",[]),None)
	     AgentMap.empty)
      )
   )
} 
    
let subs = Agent_tree.convert_declaration_into_solved_definition x
let x = 
  {agent_name = "A3" ;
      interface = ["s",("~a","~b");"y",("~c","~d")]}
  
let y = 
    {agent_name = "A3" ;
     interface = ["y","~a"]}

let z = 
    {agent_name = "A2" ;
      interface = ["s1","~b"]}


let rule1 = 
  {flag = "essai"; 
    hand_side_common=[x];
    mod_left_hand_side=[y];
    mod_right_hand_side=[z];
    fixed_left_hand_side=[];
    fixed_right_hand_side=[];
    sign="-->";
    lhs_annotation="";
    rhs_annotation="";
    rule_annotation="";} 

let rule2 = 
  {flag = "essai2"; 
    hand_side_common=[x];
    mod_left_hand_side=[y];
    mod_right_hand_side=[y];
    fixed_left_hand_side=[];
    fixed_right_hand_side=[];
    sign="-->";
    lhs_annotation="";
    rhs_annotation="";
    rule_annotation="";} 

let deal_with_rule rule = 
  let rep = Rename_rule.rename_rule rule subs in
  let _ = Printf.fprintf stdout "BEFORE\n" in
  let _ = Pretty_printing.print_rule stdout rule in
  let _ = Printf.fprintf stdout "AFTER\n" in 
  let _ = List.iter (Pretty_printing.print_rule stdout) rep in 
  ()

let _ = deal_with_rule rule1
let _ = deal_with_rule rule2 


let r = compile "essai.ka"
