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
    

(*let x = 
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
  *)  



let file = Sys.argv.(1)
let r = compile "essai.ka"
let (decl:declaration)  = Compile_directives.convert r 
let rules = List.map Compile_rule.convert r 
let decl = 
  List.fold_left 
    (fun decl x -> 
      Rename_rule.check_model 
	x decl)
    decl 
    rules 
let decl = Agent_tree.complete decl 
let subs = Agent_tree.convert_declaration_into_solved_definition decl
let rules,flags  = 
  List.fold_left 
    (fun model rule -> 
      Rename_rule.transform_model 
	rule
	subs
	model)
    ([],StringMap.empty) 
    rules 
let rules = 
  List.fold_left 
    (fun model rule -> Rename_rule.rename_obs rule flags model)
    []
    rules
let _ = Pretty_printing.print_model stdout rules



