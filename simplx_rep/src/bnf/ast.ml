open Solution
open Agent

type solution = ROOT | AGENT_SEP of agent * solution
and agent = NIL | AGENT of string * interface
and interface = EMPTY | SITE_SEP of site * interface
and site = string * link_state * internal_state
and link_state = FREE | BOUND of int | SEMI | ANY_LINK | BOUND_TYPE of string * string
and internal_state = ANY_STATE | STATE of string

let rec eval ast env ln = 
  match ast with
      ROOT -> (Solution.empty (),env)
    | AGENT_SEP ast_ag ast -> 
	let ag,pid,env = eval_agent ast_ag env ln in
	let sol = eval ast env in
	let sol = Solution.add (with_id:pid) ag sol in
	  {sol with fresh_id = max sol.fresh_id pid}

let rec eval_ag ast env ln = 
  match ast with
      NIL -> Agent.empty
    | AGENT name intf -> Agent.make name (eval_intf intf env ln)
