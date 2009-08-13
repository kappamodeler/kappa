(** THis module contains the signature for fragment modules *)
open Annotated_contact_map
open Views 
open Rooted_path
open Ode_print_sig
open Ode_print 

module type Fragments = 
    sig 
      (** type definition for fragments (i.e in canonical form)*)
      type fragment 


      (** type definition for subspecies *)
      type subspecies 


      (** type for the hashtable that is used to check compatibility *)
      type hash	    
	    
      
      (** compute the canonical form of a species and also the number of automorphisms *)	    
      val canonical_form: subspecies -> (fragment*int)
(*      val species_from_fragment: fragment -> subspecies *)

      (** value for the empty fragment *)
      val empty_fragment:fragment 
    
      (** pretty print*)
      val print_fragment: fragment -> unit

      (** value for the empty species *)
      val empty_species: subspecies 

      (** pretty print*)
      val print_species: subspecies -> unit

      val is_empty_fragment: fragment -> bool
      val is_empty_species: subspecies -> bool 

      (** If the boolean is true then these three functions associates a maximal list of compatible fragments to a bond If the boolean is false then these three functions associated a maximal list of fragments to a bond
          Each of these three functions use a different amount of memory:
	      0 -> no memoization
	      1 -> light memoization
              2 -> recursive memoization *)
      val get_denum: 
	  (views_id list Data_structures.StringListMap.t Pb_sig.StringMap.t * (views_id -> 'b Views.views) * ('a, 'b, 'c, 'd, 'e, 'f, 'g) Views.ode_handler -> 
	    (
	    (bool -> (Pb_sig.name_specie * Pb_sig.name_site * Pb_sig.name_specie * Pb_sig.name_site)  -> subspecies  list) *
	      (bool -> (Pb_sig.name_specie * Pb_sig.name_site * Pb_sig.name_specie * Pb_sig.name_site)  -> subspecies  list) *
	     (bool -> (Pb_sig.name_specie * Pb_sig.name_site * Pb_sig.name_specie * Pb_sig.name_site)  -> subspecies  list)))


     (** give the potential extensions for a subspecies *)
     val complete_subspecies: 
	 ('a -> Data_structures.String4Set.t) * (int -> 'a) *
	 (Pb_sig.String2Set.elt -> Pb_sig.String2Set.elt -> bool) *
	 (string * string * string * string -> subspecies list) ->
	   subspecies -> subspecies list

     (** extract the maximal connected components of a disconnected subspecies *)
     val split_subspecies: 
	 'a view_data_structure -> 
	   ('b,'a,'c,'d,'e,'f,'g) ode_handler -> 
	     ode_skeleton -> 
	       subspecies -> subspecies list
		   
      val plug_views_in_subspecies: string -> views_id -> subspecies -> subspecies 


(*      val from_views_id_list: views_id list -> subspecies 
      val to_views_id_list: subspecies -> views_id list *)

      val fold_views: (views_id -> 'a -> 'a) -> fragment -> 'a -> 'a 
      val scan_views: (views_id -> 'a option) -> fragment -> 'a option
      val iter_views: (views_id -> unit) -> fragment -> unit 
      val iter_views_in_species: (views_id -> unit) -> subspecies -> unit 
      val scan_views_in_species: (views_id -> 'a option) -> subspecies -> 'a option

(*      val species_of_fragment: fragment -> subspecies *)

      val get_views_from_agent_id: (views_id -> 'a views) -> string -> string -> subspecies -> (views_id * 'a views) option

      val get_neighbour: subspecies -> (string*string) -> string -> string 

      val build_species: (views_id -> string) -> views_id Pb_sig.StringMap.t -> ((string*((string*string)*(string*string))list)*views_id) list -> subspecies

      val apply_blist_with_species: ('a,'b,'c,'d,'e,'f,'g)  Views.ode_handler -> 'b Views.view_data_structure -> ((string*string) -> (string*string) -> bool) -> string -> subspecies -> (Pb_sig.b*bool) list -> (((rooted_path*string*string)*(rooted_path*string)) list) -> subspecies 


      val remove_agent_in_species: 
	  ('a,'b,'c,'d,'e,'f,'g)  Views.ode_handler -> 
	    'b Views.view_data_structure -> 
	      ((string*string) -> (string*string) -> bool) -> 
		string -> 
		  (subspecies *  (((rooted_path*string*string)*(rooted_path*string)) list))
		  -> string  
		    -> (subspecies *  (((rooted_path*string*string)*(rooted_path*string)) list))

      val merge:subspecies -> subspecies -> subspecies 

      val empty_hash: hash 
      val dump_hash: hash -> unit 
      val check_compatibility: 'a Views.view_data_structure -> hash -> subspecies -> hash*bool 

      val is_agent_in_species: string -> subspecies -> bool 

      val add_bond_to_subspecies: subspecies -> (agent_id*Pb_sig.name_site) ->  (agent_id * Pb_sig.name_site) -> subspecies 
      val release_bond_from_subspecies: subspecies -> (rooted_path*site_type)-> (rooted_path*site_type) -> subspecies 


      module FragMap: 
	  (sig 
	    type 'a t 
	    val is_empty: 'a t -> bool 
	    val empty: 'a t 
	    val find: fragment -> 'a t -> 'a
	    val add: fragment -> 'a -> 'a t -> 'a t 
	    val fold: (fragment -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
	  end) 

      module RootedFragMap:
	    (sig 
	    type 'a t 
	    val is_empty: 'a t -> bool 
	    val empty: 'a t 
	    val find: (rooted_path*int)*fragment -> 'a t -> 'a
	    val add: (rooted_path*int)*fragment -> 'a -> 'a t -> 'a t 
	    val fold: ((rooted_path*int)*fragment -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
	  end)

      val root_of_species: subspecies -> (rooted_path * int)
      val pretty_print: print_desc -> fragment -> Tools.string_handler -> ('a,'b,'c,'d,'e,'f,'g) Views.ode_handler -> 'b Views.view_data_structure  -> 
	(string*string -> string*string -> bool) -> string option -> bool -> unit 
end
