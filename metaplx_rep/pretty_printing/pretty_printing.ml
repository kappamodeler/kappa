open Data_structures_metaplx 

let print_agent log a = 
  let print_string s = Printf.fprintf log "%s" s in
  let print_newline () = Printf.fprintf log "\n" in 
  print_string a.agent_name;
  print_string "(";
  List.fold_left  
    (fun bool s -> 
      let _ = if bool then print_string "," in 
      let _ = print_string (fst s);print_string (snd s) in 
      true)
    false a.interface;
  print_string ")"

let print_agent_list log l bool = 
  List.fold_left 
    (fun bool agent -> 
      let _ = if bool then Printf.fprintf log " , " in
      let _ = print_agent log agent in
      true)
    bool l 


      

let print_rule log rule = 
  let mapsite f = 
    List.map 
      (fun agent -> {agent with interface = List.map (fun (a,b) -> (a,f b)) agent.interface}) in 
  let _ = if rule.flag <> "" then Printf.fprintf log "\'%s\' " rule.flag in 
  let commonl = mapsite fst rule.hand_side_common in 
  let commonr = mapsite snd rule.hand_side_common in 
  let bool = print_agent_list log commonl false in 
  let bool = print_agent_list log rule.mod_left_hand_side bool in 
  let bool = print_agent_list log rule.fixed_left_hand_side bool in 
  let _ = Printf.fprintf log "%s %s " rule.lhs_annotation rule.sign in 
  let bool = print_agent_list log commonr false in 
  let bool = print_agent_list log rule.mod_right_hand_side true in 
  let bool = print_agent_list log rule.fixed_right_hand_side true in 
  let _ = Printf.fprintf log "%s %s" rule.lhs_annotation rule.rule_annotation in 
  let _ = Printf.fprintf log "\n" in 
  () 
