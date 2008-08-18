(**Module to handle any agent based operation*)

(**Type for site states.*)
type mark = 
    Marked of string  (**Internal state*)
  | Wildcard (**Any state*)
  | Bound (**Bound site*)
  | Free (**Free site*)

type environment 

(**Type for agents*)
type t (*= {name:string; (**name of agents*)
	  interface:Mods.StringSet.t; (**Interface of agents*)
	  state_of_site: agentmap  (*string State.t*) (**map from site to [mark,mark] corresponding to internal and link state *)
	 } *)

(**The empty agent*)
val empty : t

(**Returns [true] if agent is empty*)
val is_empty : t -> bool

(**Returns the name of the agent*)
val name : t -> string

(**[state ag x] returns the pair of marks [(mrk,mrk')] corresponding to the internal and link states of site [x] of agent [ag]*)
val state : t -> string -> mark * mark


(**[interface ag] returns the interface of agent [ag]. The interface is a set of strings.*)
(*val interface : t -> Mods.StringSet.t*)
val interface: t -> string list
(**[mark ag x mrk] returns the agent [ag] in which internal state of site [x] has been set to [(Marked mrk)]. If site [x] does not exist [Error.Runtime] is raised.*)
val mark : t -> string -> string -> t


(**[modif ag x] will set [x] to [Bound] if it was free and conversly. If site [x] does not exist [Error.Runtime] is raise.*)
val modif : t -> string -> t

(**[compatible_mark pattern mark mark'] returns [true] if marks can be unified. The only valid unifications are [(Wildcard,_) (_,Wildcard) (Free,Free) (Bound,Bound)] and [(Marked mrk,Marked mrk)]*)
val compatible_mark : ?strict:bool -> mark -> mark -> bool

(**Converts an agent into a string*)
val to_str : ?ordered:bool -> t -> string

(**test wheter an interface is empty*)
val is_empty_intf : t  -> bool

(**[make name set map] creates an agent whose sites are given by set. The map should send a site in set to a pair of marks*)
val make : string -> Mods2.StringSet.t -> environment  -> t

val fold_interface: (string -> mark*mark -> 'a -> 'a) -> t -> 'a -> 'a 
val fold2iz_interface: (string -> mark*mark -> mark*mark -> 'a -> 'a) -> t -> t -> 'a -> 'a
val fold_environment: (string -> mark*mark ->'a -> 'a) -> environment -> 'a -> 'a
val add_environment: string -> mark*mark -> environment -> environment 
val empty_environment: environment 
val mem_environment: string -> environment -> bool 
val mem_interface: string -> t -> bool 
val detach: t -> t
