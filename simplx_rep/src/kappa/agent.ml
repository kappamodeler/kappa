open Error
open Mods2

type mark = Marked of string | Wildcard | Bound | Free 


(* To share agents in the solution *)
(*  
module Agents_imperative = Map_ext.Make(struct type t = string let compare = compare end)
    
module Agents = Agents_functional_sharing

type agentmap = (mark*mark) Agents.t *)

let switch state s =
  match state with
      (Marked _ | Wildcard) -> Marked s
    | Bound -> Free
    | Free -> Bound
(*    | Wildcard -> raise (Error.Runtime "Agent.switch: cannot switch a wildcard")*)

(*module SharedStringMap = Map_with_sharing.Make_sharing(struct type t = string type t'= mark *mark let compare = compare end)*)
module SharedStringMap = Map2.Make(struct type t = string type t' = mark*mark let compare = compare end)

type environment = (mark*mark) SharedStringMap.t 

type t = {name:string;(*interface:StringSet.t;*)
	  state_of_site: environment  } (*nom,interface,(site->etat)*)

let empty = {name="%NIL%";state_of_site= SharedStringMap.add "_" (Wildcard,Free) SharedStringMap.empty}

let environment a = (*SharedStringMap.tree_of_int*) a.state_of_site

let is_empty ag = (ag.name = "%NIL%")
let name ag = if is_empty ag then "%NIL%" else ag.name  

let state ag x = 
  try 
    SharedStringMap.find x (environment ag)
  with Not_found -> 
    let error = Printf.sprintf "Agent.state: site %s not found in agent %s" x (name ag) in
      raise (Runtime error)

let mark ag x s = 
  let (inf,lnk) = state ag x in
    {ag with state_of_site = (*SharedStringMap.int_of_tree*) (SharedStringMap.add x (switch inf s,lnk) (environment ag)) }

let modif ag x =
  let (inf,lnk) = state ag x in
    {ag with state_of_site = (*SharedStringMap.int_of_tree*) (SharedStringMap.add x (inf,switch lnk "") (environment ag))} 

exception Strict

let compatible_mark ?(strict=false) m_inst m_pat =
  match (m_inst,m_pat) with
      (Wildcard,_) -> 
	if strict then 
	  if (m_pat=Wildcard) then true 
	  else false 
	else true
    | (_,Wildcard) -> true
    | (Free,Free)|(Bound,Bound) -> true
    | (Marked s,Marked s') -> (s=s')
    | _ -> false

let compare_state s1 s2 =
  match (s1,s2) with
      (Bound,Bound)|(Free,Free)|(Wildcard,Wildcard) -> 0
    | (Marked s,Marked s') -> compare s s'
    | (Bound,Free) | (Marked _,Wildcard) -> 1
    | _ -> (-1) 

let to_str ?(ordered=false) ag = 
  let l = SharedStringMap.fold (fun site (inf,lnk) cont ->
				  if site = "_" then cont else
				    let s_inf =
				      match inf with
					  Wildcard -> ""
					| Marked s -> "~"^s
					| _ -> invalid_arg "Agent.to_str"
				    in
				    let s_lnk = 
				      match lnk with
					  Bound -> "!"
					| Free -> ""
					| Wildcard -> "?"
					| _ -> invalid_arg "Agent.to_str"
				    in
				      (site^s_inf^s_lnk)::cont
			       ) (environment ag) []
  in
  let l = if ordered then List.fast_sort compare l else l in
    Printf.sprintf "%s(%s)" ag.name (String.concat "," l)

let rec is_empty_intf intf = 
  let a = environment intf in
  let a' = SharedStringMap.remove "_" a in
   SharedStringMap.is_empty a'


let make name interface state_of_site = 
  let state_array = 
    (*SharedStringMap.int_of_tree*) (SharedStringMap.fold (fun x s ar -> SharedStringMap.add x s ar) state_of_site SharedStringMap.empty) in
    {name = name ; state_of_site = state_array}


let fold_interface f agent  =  SharedStringMap.fold f (environment agent) 
let fold2iz_interface f agent1 agent2  = SharedStringMap.fold2i f 
(environment agent1) (environment agent2) 
let fold_environment = SharedStringMap.fold
let add_environment = SharedStringMap.add 
let empty_environment = SharedStringMap.empty 
let mem_environment a b = 
  try (SharedStringMap.find a b;true) with Not_found -> false 
let mem_interface a b  = mem_environment a (environment b)

let interface ag = 
  List.rev 
    (fold_interface 
       (fun a _ sol -> a::sol) 
       ag [])

let detach ag =
  fold_interface (fun site (_,lnk) ag -> 
		    if lnk = Bound then modif ag site else ag
		 ) ag ag

let factice ag = ag.name = "%NIL%"
