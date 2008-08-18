(**Module for managing rule-based operations*)

(**Type of rules*)
type t = {
  lhs : Solution.t Mods2.IntMap.t; (**Rule left hand side is a map 0->sol_0... n->sol_n where each sol_i is a connected solution of the left hand side of the rule*)
  rhs : Solution.t (**Rule right hand side is just a solution*);
  precompil :(int * Solution.cc_recognition * int Mods2.IntMap.t) list Mods2.IntMap.t; (**[precompil] maps each connected component number [j] (which is also an entry in the [lhs] map) to a list of the form [[(id_i,reco_i,role_map_i);...]] where [id_i] is the identifier of the agent in solution [lhs(j)] that is the root of the recognitions instructions in [reco_i] (so it needs to be matched first). [role_map_i] is the association that maps identifiers of agents in [lhs(j)] to their role in [reco_i]*)
  add : Agent.t Mods2.IntMap.t; (**[add] is a map of the form [0->ag_0;...;n->ag_n] where [ag_i] is an agent added by the rule. *)
  actions : (Solution.action * Solution.msg) list Mods2.IntMap.t; (**[actions] is a map of the form [i->[(action_i1,msg_i1);...;(action_iN,msg_iN)]] where [i] is a role in the role_map, and action is the action to be performed on the agent pointed by [role_map(i)] upon application of the rule. [msg] is just a warning message. Note that each role may have several actions to execute. For instance in [->a(x!1),b(x!1)] the agents have to be created and then bound together.*)
  corr_ag : int; (**The number of agent that the rule is creating (may be negative)*)
  rate : int; (**Rate which is indicating whether the rule has a warning or not --old*)
  input : string; (**Kappa syntax of the rule*)
  flag : string option; (**If [flag] is [Some str] then str denotes the name that was given to the rule upon creation*)
  constraints : Solution.constraints list; (**Constraints list for application of the rule*)
  kinetics : float; (**Kinetic rate of the rule (-1) denotes infinite rate*)
  automorphisms : int option ; (**Number of automorphisms of the lhs *)  
  n_cc : int; (**[n_cc] is the number of connected component in the lhs of the rule --old*)
  id : int; (**Identifier of the rule in the rule set*)
  infinite : bool; (**boolean which is true when the rule has infinite rate*)
  abstraction :  t list option (**pointer to the rules that are the most generic refinements of this rule*) 
}

type rule_type=t (**alias for Rule.t*)
type marshalized_t (**type for marshalized rule*)
val marshal : t -> marshalized_t (**Turning a rule into a purely functional type*)
val unmarshal : marshalized_t -> t (**Turning a marshalized rule back to a rule type*)
val empty : t (**Empty rule*)

(**Module for managing rule sets*)
module Rule_of_int : 
  sig
    type key=int (**Type of rule identifier --here just integer*)
    type t (**Type of rule set*)
    type val_type = rule_type * float 
    val empty : int -> t (**Empty rule set*)
    val copy : t -> t (**[copy rule_set] returns a copy of [rule_set] with no shared references*)
    val add : key -> val_type -> t -> t (**[add key (r,v) rule_set] returns a rule set identical to rule_set+(r,v). NB: add should always be used in the following way: [let rule_set = add key (r,v) rule_set] since the returned rule_set will share references with the previous one.*)
    val find : key -> t -> val_type (**[find key rule_set] returns the pair [(r,v)] of rule and values associated with [key] in the rule set.*)
    val restore_consistency : t -> t
    val iter : (key -> val_type -> unit) -> t -> unit (**Iteration on rule sets*)
    val fold : (key -> val_type -> 'a -> 'a) -> t -> 'a -> 'a (**fold on rule sets*)
    val random_val : t -> key * val_type (**Returns a pair [(key,(r,val))] according the probability distribution given bay values in the set*)
    val accval : t -> float (**Returns the sum of all values in the set --constant time*)
  end

val print_compil : ?filter:bool -> ?with_warning:bool -> t -> unit (**[print_compil ~b1 ~b2 r] prints the compilation of the rule on standard output. Booleans [b1] and [b2] respectively allow one to prevent being too verbose and to display warning*)
val rhs : t -> Solution.t (**[rhs r] is identical to [r.rhs]*)
val flag : t -> string option (**[flag r] is identifcal to [r.flag]*)
val name : t -> string (**[string_of_rule r] just returns r.input*)
val mod_quarks : t -> Mods2.PortSet.t * Mods2.IntSet.t (**[mod_quarks r] returns the pair [(mod_sites,mod_ids)] of which are respectively the set of sites modified by the rule and the set of agent identifiers mdoified by the rule. [mod_sites] is a set of the form [{(id_1,s_1),...,(id_n,s_n)}] where [id_i] is an agent identifier in the solution and [s_i] is a string of the form "xx~" when denoting the internal state of site [xx] of agent [id_i] and "xx!" when denoting the link state of site [xx] of agent [id_i]. *)
val ( << ) : t -> t -> bool (**[r<<r'] is [true] when rule [r] has a positive influence on rule [r']*)
val ( %> ) : t -> t -> bool (**[r%>r'] is [true] when rule [r] has a negative influence rule [r']*)

(**Modification types for quarks that are passed to the story sampler*)
type modif_type =
    Bound of (int * string) (**[Bound (id,"s!")] indicates that the corresponding site is bound to the site [s] of agent [id]*)
  | Break of (int * string) (**[Break (id,"s!")] indicates that the corresponding site is detached from the the site [s] of agent [id] to which it was bound*)
  | Side_break of (int * string) (**Modif type in the case of a rule which has side effect. The only such rules are of the form (1) [a()->] in a context of the form [a(y!1),b(x!1)] --in which case the site x of be will be marked as [Side_break (i,y!)] where [i] is the identifier of a in the solution and (2) [a(x!_)->a(x)] in a similar context.*)
  | Marked of (string * string) (**[Marked ("s~","m")] indicates that the corresponding site has been marked with internal state "m"*)
  | Remove (**mark for removal*)
  | Init_mark of (string * string) (**[Init_mark "s~","m"] mark for internal state quark introduction*)
  | Init_free of string (**[Init_free "s!"] mark for link state quark introduction*)
  | Init_bound of (string * int * string) (**[Init_bound "s!",i,"s'!"] indicates that the quark ["s!"] is introduced already bound to quark ["s'!"] of agent [i]*)
  | Test_bound of (int * string) (**[Test_bound i,"s!"] indicates that quark connection to quark [(i,s)] is being tested by event*)
  | Test_marked of string (**[Test_marked s] indicates that the internal state quark is being tested by event*)
  | Test_any_bound (**[Test_marked] indicates that quark connection is being tested by event*)
  | Test_free (**[Test_marked] indicates that quark freedom is being tested by event*)
  | Before_After of (modif_type * modif_type) (**For story compression purpose*)

(**[is_pure_test modif_list] is [true], if [modif_list] only contains tests *)
val is_pure_test : modif_type list -> bool

(**[contains_test modif_list] is [true], if [modif_list] contains tests *)
val contains_test : modif_type list -> bool

(**[contains modif_list] is the negation of [is_pure_test]*)
val contains_modif : modif_type list -> bool

(**[is_creation modif_list] is [true] if lists of modification correspond to the creation of an agent*)
val is_creation : modif_type list -> bool

(**[is_deletion modif_list] is [true] if lists of modification correspond to the deletion of an agent*)
val is_deletion : modif_type list -> bool

(**Converts a modif into a string for pretty printing purpose*)
val string_of_modif_type : modif_type -> string

(**[apply r inj sol] applies rule [r] via injection [inj] to solution [sol]. The result is a tuple [(mq,rmq,tq,map_add,sol',w)]
where [mq,rmq,tq] are the sets of quarks in [sol] that are respectively modified, deleted or tested by the rule application,
[map_add] is mapping agent keys in [r.add] to agent identifiers in [sol'],
[sol'] is the solution resulting from the application of [r] to [sol] via [inj] and [w] is a warning that is set to [true] if rule application had side effects.*)
val apply :
  t ->
  int Mods2.IntMap.t ->
  Solution.t ->
  modif_type list Mods2.PortMap.t * Mods2.PortSet.t *
  modif_type list Mods2.PortMap.t * int Mods2.IntMap.t * Solution.t * bool

(**Pretty printing of rule card on standard output*)
val print : t -> unit

exception Opposite of modif_type list Mods2.PortMap.t
exception Not_opposite
val opposite :
  modif_type list Mods2.PortMap.t ->
  modif_type list Mods2.PortMap.t -> Solution.t -> unit
val str_of_actions : t -> string
val to_dot : t -> string
val injections_product :
  'a Mods2.AssocArray.t list ->
  (Mods2.IntSet.t * Mods2.IntSet.elt Mods2.IntMap.t) list
(*val automorphism : t -> int*)
val contains_deletion : t -> bool
val subs_act : Mods2.IntMap.key Mods2.IntMap.t -> modif_type -> modif_type
val subs_rule :
  Mods2.IntMap.key Mods2.IntMap.t ->
  'a * modif_type Mods2.PortMap.t -> 'a * modif_type Mods2.PortMap.t
