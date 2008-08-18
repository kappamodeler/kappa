let rule_select_time = ref 0.0
let rule_apply_time = ref 0.0
let update_time = ref 0.0
let data_time = ref 0.0
let neg_upd = ref 0.0
let pos_upd = ref 0.0
let t_upd_rules = ref 0.0

let output() = 
  Printf.fprintf stderr "**Rule selection time: %.4f CPU\n**Rule application time: %.4f CPU\n**Update phase time: %.4f CPU\n**Positive upd time: %.4f CPU\n**Negative upd time: %.4f CPU\n**Update of rule map: %.4f CPU\n" 
    !rule_select_time !rule_apply_time !update_time !pos_upd !neg_upd !t_upd_rules;
  flush stderr

    
