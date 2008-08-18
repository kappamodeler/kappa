module type Content = 
  sig
    type t
    val default:t
  end

module type Array_ext = 
    sig
      type content 
      type 'a t 
      val copy : 'a t -> 'a t
      val size : 'a t -> int
      val create : int -> 'a t
      val remove : 'a -> 'a t -> 'a t
      val remove_index : int -> 'a t -> 'a t
      val add : 'a -> content -> 'a t -> 'a t
      val find : 'a -> 'a t -> content
      val iter : ('a -> content -> unit) -> 'a t -> unit
      val fold : ('a -> content -> 'b -> 'b) -> 'a t -> 'b -> 'b
      val random : 'a t -> 'a * content
      val is_empty : 'a t -> bool
      val mem : 'a -> 'a t -> bool
    end


module Make(T:Content) = 
(struct    
   type content = T.t
   type 'a t = {mutable fresh: int ; ar: ('a option * content) Big_array.BigArray.t ; hsh:('a,int) Hashtbl.t}
       
   let get = Big_array.BigArray.get
   let set = Big_array.BigArray.set

   let copy a = {fresh = a.fresh ; ar = Big_array.BigArray.copy a.ar ; hsh = Hashtbl.copy a.hsh}
     
   let size a = a.fresh (*virtual size of the extensible array*)

   let space a = Big_array.BigArray.length a.ar (*real length of the array*)
    
   let create size = 
     if (size < 0) or (size >= Sys.max_array_length) then invalid_arg "Array_ext.create" 
     else 
       {ar = Big_array.BigArray.create size (None,T.default) ; fresh = 0 ; hsh = Hashtbl.create size}

   let key_of_opt opt = 
     match opt with
	 Some k -> k
       | None -> invalid_arg "Array_ext.key_of_opt"

   let remove key a = 
     try
       let i = Hashtbl.find a.hsh key in
	 if a.fresh = 0 then invalid_arg "Array_ext.remove" 
	 else
	   let (opt,v) = get a.ar (a.fresh-1) in (*get key and value of last element in array*)
	   let k_v = key_of_opt opt in
	   let _ =
      	     set a.ar i (opt,v) ; (*one replaces the values of the erased cell*)
	     Hashtbl.replace a.hsh k_v i ; (*k_v now refers to adress i in the array*) 
	     Hashtbl.remove a.hsh key ;
	     set a.ar (a.fresh-1) (None,T.default) ; (*not really necessary*)
	     a.fresh <- a.fresh-1 
	   in
	     a 
     with Not_found -> a
       | Invalid_argument msg -> raise (Invalid_argument ("Array_ext.remove: "^msg))

   let remove_index k a = 
     match fst (get a.ar k) with 
	 Some key -> (let _ = remove key a in a) 
       | None -> a

   let add key v a =
     try
       let ind,is_new = 
	 try (Hashtbl.find a.hsh key,false)
	 with Not_found -> (Hashtbl.add a.hsh key a.fresh ; (a.fresh,true))
       in
	 if ind = space a then
	   (*if ind > (Sys.max_array_length-1) then Error.expected "Max array size reached, aborting..."
	     else
	     let size' = min (2 * (size a + 1)) Sys.max_array_length in
	   *)
	   let size' = 2 * (size a + 1) in
	   let ar' = Big_array.BigArray.init size' (fun i -> 
						      if i < ind then get a.ar i (*copying old values*)
						      else 
							(None,T.default) (*default value*)
						   ) 
	   in
	     (a.fresh <- a.fresh + 1 ; set ar' ind (Some key,v) ; {a with ar = ar'})
	 else (set a.ar ind (Some key,v) ; if is_new then a.fresh <- (a.fresh + 1) ; a)
     with 
	 Invalid_argument msg -> raise (Invalid_argument ("Array_ext.add: "^msg))

  let find key a = (*may raise Not_found*)
    let i = Hashtbl.find a.hsh key in
      if i < a.fresh then (fun (_,y) -> y) (get a.ar i) else invalid_arg "Array_ext.find"

  exception Stop 

  let iter f a = 
    try 
      Big_array.BigArray.iter (fun (opt,v) -> match opt with None -> raise Stop | Some k -> f k v) a.ar
    with Stop -> ()

  let fold f a seed = 
    let i = ref 0 in
    let cont = ref seed in
    let _ = 
      while !i < (size a) do
	let (opt,v) = get a.ar (!i) in
	  match opt with
	      Some k -> 
		begin
		  cont := (f k v !cont) ;
		  incr i
		end
	    | None -> failwith "Array_ext.fold: value doesn't match any key"
      done 
    in
      !cont

  let random a = 
    if a.fresh < 1 then invalid_arg "Array_ext.random" 
    else 
      let opt,v = get a.ar (Random.int a.fresh) in
	match opt with
	    Some k -> (k,v)
	  | None -> failwith "Array_ext.random: value doesn't seem to be matched to a key"

  let is_empty a = (a.fresh = 0)

  let mem key a = 
    try
      let i = Hashtbl.find a.hsh key in (i < a.fresh) (*should always be the case*)
    with Not_found -> false


end:Array_ext with type content = T.t)

