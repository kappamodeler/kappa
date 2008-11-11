(**Module for all solution based manipulations*)

module AA : (**Array of agents*)
sig
  type content = Agent.t
  type 'a t
  val copy : 'a t -> 'a t
  val size : 'a t -> int
  val create : int -> 'a t
  val remove : 'a -> 'a t -> 'a t
  val remove_index: int -> 'a t -> 'a t 
  val add : 'a -> content -> 'a t -> 'a t
  val find : 'a -> 'a t -> content
  val iter : ('a -> content -> unit) -> 'a t -> unit
  val fold : ('a -> content -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val random : 'a t -> 'a * content
  val is_empty : 'a t -> bool
  val mem : 'a -> 'a t -> bool
end

module PA : (**Array of ports (pairs of agent name and site)*)
sig
  type content = (int*string)
  type 'a t
  val copy : 'a t -> 'a t
  val size : 'a t -> int
  val create : int -> 'a t
  val remove : 'a -> 'a t -> 'a t
  val add : 'a -> content -> 'a t -> 'a t
  val find : 'a -> 'a t -> content
  val iter : ('a -> content -> unit) -> 'a t -> unit
  val fold : ('a -> content -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val random : 'a t -> 'a * content
  val is_empty : 'a t -> bool
  val mem : 'a -> 'a t -> bool
end

(**The type of solutions*)
type t = {
  agents : int AA.t; (**Indexed extensible array of agents in the solution*)
  links : (int*string) PA.t; (**Edges represented by symmetric association of the form (id,site) -> (id',site')*)
  fresh_id : int; (**Fresh id for next agent*)
}

type marshalized_t (**For for marshalized solutions*)

(**[marshal sol] returns a purely functionnal solution that can be safely marshalized*)
val marshal : t -> marshalized_t

(**[unmarshal f_sol] takes a purely functional solution and converts it to a solution*)
val unmarshal : marshalized_t -> t

(**The empty solution*)
val empty : unit -> t

(**Copying a solution --needed because of imperative fields*)
val copy: t -> t

(**Returns the map of agents of the solution*)
val agents : t -> int AA.t

(**Returns the next fresh id of the solution*)
val fresh_id : t -> int

(**Test whether a solution is empty*)
val is_empty : t -> bool

(**[get_port (id,site) sol] returns the pair [(id',site')] if the edge [(id,site),(id',site')] exists and raise [Not_found] otherwise*)
val get_port : int * string -> t -> int * string

(**[agent_of_id id sol] returns the agent with id [id] in solution [sol]. Raise [Not_found] if [id] is not an existing agent id*)
val agent_of_id : int -> t -> Agent.t

(**[kappa_of_solution sol] converts solution [sol] into a string corresponding to a kappa term. If the optional field [full] is [true] then agents in the string are concatenated with their id.*)
val kappa_of_solution : ?full:bool -> ?whitelist:Mods2.IntSet.t -> t -> string


(**[to_dot sol] converts solution [sol] to a string corresponding to a graph in dot format. If the optional field [low_reso] is [true] then the dot string will not contain decoration of edges with site names*)
val to_dot : ?low_reso:bool -> t -> string

(**[to_dot2 title cc_map] converts the map [cc_map] of connected solutions into a graph in dot format containing the clusters corresponding to each connected components. The graph will have [title] as title.*)
val to_dot2 : string -> t Mods2.IntMap.t -> string

(**[bind (id,site) (id',site') sol] returns a solution corresponding to [sol] in which the new edge [(id,site) (id',site')] has been added. Raise [Error.Runtime] if one of the site is already bound*)
val bind : int * string -> int * string -> t -> t

(**[unbind (id,site) (id',site') sol] returns a solution corresponding to [sol] in which the edge [(id,site) (id',site')] has been removed. Raise [Error.Runtime] if the edge does not exists.*)
val unbind : int * string -> int * string -> t -> t

(**[mark (id,site,mrk) sol] returns a solution corresponding to [sol] in which the internal state of site [site] of agent [id] has been set to the string [mrk]*)
val mark : int * string * string -> t -> t

(**[add ag sol] adds agent [ag] to the solution [sol]. A fresh id is attributed for [ag] unless the optional field [with_id] is provided, in which case the given identifier will be used (without test)*)
val add : ?with_id:int -> Agent.t -> t -> t

(**[remove id sol] removes agent identified by [id] from the solution [sol] and returns a pair [(warn,sol')] where warn is true if removing involved breaking additional bounds.*)
val remove : int -> t -> (bool * t)

(**Map from role in the recognition to instructions*)
type cc_recognition 

val empty_reco:cc_recognition

val string_of_recognition : cc_recognition -> string

(**[get_instructions id reco map fresh sol] computes the necessary instructions to recognise agent [id] in solution [sol].  returns [(reco',map',fresh')] where [reco'] is the updated recognition, [map'] is the updated map from identifier to role in the recognition and [fresh'] is the next role to be attributed*)
val get_instructions : 
  int -> cc_recognition -> int Mods2.IntMap.t -> int -> t 
  -> (cc_recognition * int Mods2.IntMap.t * int)

(*
(**[connected_component id sol] returns the set of identifiers which belong to the connected component of [id] in [sol]*)
val connected_component : ?exclude:(Mods2.StringSet.t) -> int -> t -> Mods2.IntSet.t
val connected_names : int -> t -> int -> Mods2.StringSet.t -> Mods2.StringSet.t
*)
(**[split sol] returns a map of solutions corresponding to the connected components of [sol]*)
val split : t -> t Mods2.IntMap.t

(**[recognitions_of_cc cc] returns a list of all possible recognitions of [cc] starting at a different entry point in [cc]. It returns a list of triple of the form [(id_i,reco_i,role_of_id_i)] where [reco_i] is the ith recognition of the connected component [cc] starting at agent [id_i] and where [role_of_id_i] is the association from identifiers in [cc] to roles in [reco_i]*)
val recognitions_of_cc : ?rooted:bool -> t -> (int * cc_recognition * int Mods2.IntMap.t) list

exception Matching_failed

(**[match_id_with_role id role reco sol] checks whether the connected component of agent [id] in solution [sol] matches the recognition [reco]. In case of success the function returns the map from identifiers in [sol] to roles in [reco] and raises [Matching_failed] otherwise.*)
val match_id_with_role :
    ?pushout:bool ->
    int ->
    int ->
    cc_recognition ->
    t -> int Mods2.IntMap.t

(**let [reco_cc] be obtained by a call to [recognitions_of_cc cc] and [agents] be a subset of the indexed set of agents of a solution [sol], then [unify reco_cc (agents,sol)] returns [assoc_map] which is an indexed set of the form [0 -> map_0 ... n -> map_n ] where each [map_i] is a map from identifiers in [cc] to identifiers in [sol] corresponding to injections of [cc] into [sol]. The injections are obtained using agents specified in [agents]. The [unify] function accepts an optional boolean argument [~rooted] the effect of which is to limit the unification a single entry point in [reco_cc] (instead of using all possible entry points)*)
val unify:
  ?rooted:bool 
  -> ?pushout:bool
  -> (int * cc_recognition * int Mods2.IntMap.t) list * t 
  -> int AA.t * t 
  -> (*int Mods2.IntMap.t Mods2.IntMap.t*) int Mods2.AssocArray.t

(**[is_instanciated id sol] is [true] if the agent identified by [id] in solution [sol] contains no dangling link or wildcard link*)
val is_instanciated : int -> t -> bool

(**Modification actions of a solution*)
type action =
    Bind of (string * int * string) (**[Bind (site,id,site')]*)
  | Break of (string * int * string) (**[Break (site,id,site')]*)
  | Mark of (string * string) (**[Mark (site,mark)]*)
  | Modify of string (**[Modify site] breaks a dangling link from site [site]*)
  | Remove (**[Remove] remove the agent*)

(**Converts an action into a string for compilation output*)
val string_of_action : int -> action -> string

(**Message attached to actions on a solution if necessary*)
type msg = Warning of string | OK

(**[diff sol sol'] returns [(map,add_sol,rate,corr)] where [map] is a is a map from id in [sol] to a list [act_msg_list] of the form [[(act_1,msg_1);...;(act_n,msg_n)]] corresponding to the actions to apply in order to transform [sol] into [sol']. [add_sol] is a map form identifier in sol' to agents which are not present in [sol], [rate] is an integer which counts the number of actions necessary to transform [sol] into [sol'] and [corr] is the difference between the number of agents in [sol] and the number of agents in [sol']*)
val diff : t -> t -> (action * msg) list Mods2.IntMap.t * Agent.t Mods2.IntMap.t * int * int

(*
(**Constraints in the application of a rule: [CC pid set_joints set_disjoints] allows to declare that the image of agent [pid] should not be in the connected component of the image of agents in [set_disconnect]. On the contrary images of agents in [set_connect] should be in the connected component of the image of [pid]. *)
type constraints = CC of (int * Mods2.IntSet.t * Mods2.IntSet.t * Mods2.StringSet.t)
(**[satisfy cstr_l inj sol] checks whether the constraints in list [cstr_l] are satisfied in solution [sol] according to injection [inj]*)
val satisfy : constraints list -> int Mods2.IntMap.t -> t -> bool
*)

(**[compose sol sol'] returns a solution that is the sum of [sol] and [sol']. Because of side effects in imperative parts of [sol], that function should always be used as [let sol = compose sol sol']*)
val compose : t -> t -> t

(**[multiply sol n] multiplies the agents of [sol] by factor [n]. Because of side effects in imperative parts of [sol], that function should always be used as in [let sol = multiply sol 10]*)
val multiply : t -> int -> t

(**[pushout cc1 cc2] returns either [None] or [Some map] where map  *)
val pushout : t -> t -> int Mods2.IntMap.t list option

val injections_product : int Mods2.AssocArray.t (*Mods2.IntMap.t Mods2.IntMap.t*) list -> int Mods2.IntMap.t Mods2.IntMap.t

val fuse_cc : t Mods2.IntMap.t -> t

(**[cc_of_id id sol] returns the solution corresponding to the connected component of id in sol*)
val cc_of_id : int -> t -> Mods2.IntSet.t -> (t * Mods2.IntSet.t)

type observation = Concentration of (string * t) | Occurrence of string | Story of string

type marshalized_obs = FConcentration of (string * marshalized_t) | FOccurrence of string | FStory of string

val marshal_obs : observation -> marshalized_obs
val unmarshal_obs : marshalized_obs -> observation 
val paths_of_id : int -> t -> (Mods2.IntSet.t * Paths.t)
val get_binding : (action * msg) list -> (string * int * string) list
val sol_of_init: bool -> (t * int) list -> t
val insert_empty_agent : t -> t

