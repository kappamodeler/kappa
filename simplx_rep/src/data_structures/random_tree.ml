module type Random_tree = 
    sig
      type tree 
      type val_type 
      val create:int -> tree
      val copy:tree -> tree
      val copy_in: tree -> tree -> tree
      val update_event: int -> float -> tree -> tree 
      val random: tree -> int
      val update_structure: tree -> tree
    end

module Random_tree = 
  struct 
    type tree = 
	{size:int;
	  weigth_of_nodes:float array ;
	  weigth_of_subtrees:float array ; 
	  unbalanced_events_by_layer:int list array ;
	  unbalanced_events:bool array;
          layer:int array ;
	  mutable consistent:bool}

    let copy t = 
      { size = t.size;
	weigth_of_nodes=Array.copy t.weigth_of_nodes ;
	weigth_of_subtrees=Array.copy t.weigth_of_subtrees ;
	layer = Array.copy t.layer;
	consistent = t.consistent ;
	unbalanced_events_by_layer=Array.copy t.unbalanced_events_by_layer ;
	unbalanced_events=Array.copy t.unbalanced_events}

    let copy_vect_in t t1 = 
      let _ = Array.iteri (fun i a -> t1.(i)<-a) t in
      t1

    let copy_in t1 t2 = 
      let _ = copy_vect_in t1.weigth_of_nodes t2.weigth_of_nodes in
      let _ = copy_vect_in t1.weigth_of_subtrees t2.weigth_of_subtrees in
      let _ = copy_vect_in t1.layer t2.layer in
      let _ = copy_vect_in t1.unbalanced_events t2.unbalanced_events in
      let _ = copy_vect_in t1.unbalanced_events_by_layer t2.unbalanced_events_by_layer in
      let _ = t2.consistent<-t1.consistent in
      t2 

    let dump t = 
      let _ = print_string "SIZE:" in
      let _ = print_int t.size in
      let _ = print_newline () in 
      let _ = 
	if t.consistent then print_string "CONSISTENT\n" in 
	  
      let _ = print_string "weigth of nodes/subtrees" in
      let _ = print_newline () in
      let _ = 
	let rec aux k = 
	  if k>t.size then ()
	  else (print_int k;
		print_string ":";
		print_float (t.weigth_of_nodes.(k));
		print_string ";";
		print_float (t.weigth_of_subtrees.(k));
		print_newline ();
		aux (k+1))
	in aux 0 in () 

    let pere i = i/2 
    let left_son i = i*2
    let right_son i = i*2+1
	
    let is_left_son i = i mod 2 = 0 
    let is_right_son i = i>1 && i mod 2 = 1	 
    let is_root i = i=1 


    let rec update_structure t = 
      if t.consistent = false then 
	let n_layer = t.layer.(t.size) in 
	let weigth_of_subtree k = 
	  if k>t.size then 0.
	  else t.weigth_of_subtrees.(k) in 
	let rec aux k = 
	  if k = 0 then () else 
	  let l = t.unbalanced_events_by_layer.(k) in
	  let _ = t.unbalanced_events_by_layer.(k) <- [] in 
	  let _ = 
	    List.iter      
	      (fun i -> 
		let _ = 
		  t.weigth_of_subtrees.(i)<-
		    t.weigth_of_nodes.(i)
		      +. weigth_of_subtree (2*i) 
		      +. weigth_of_subtree (2*i+1)
		in
		let _ = t.unbalanced_events.(i) <- false in
		let _ = 
		  if is_root i 
		  then ()
		  else 
		    let father = i/2 in
		  let _ = declare_unbalanced father t 
		  in () 
		in ()) l
	  in 
	  aux (k-1) in
	let _ = aux n_layer in
	let _ = t.consistent<-true
	in t 
      else t
    and
	declare_unbalanced i t = 
      let _ =
	if t.unbalanced_events.(i) 
	then ()
	else
	  let l = t.layer.(i) in
	  let _ = t.unbalanced_events.(i)<-true in  
	  t.unbalanced_events_by_layer.(l)
	  <- i::(t.unbalanced_events_by_layer.(l))
      in
      let _ = t.consistent<-false in
      t 

	
    let create n  = 
      let t_node  = Array.create (n+1) 0. in
      let t_subtree = Array.create (n+1) 0. in 
      let layer = Array.create (n+1) 0 in 
      let _ = 
	let rec aux k current_layer layer_end = 
	  if k>n then () 
          else if k>layer_end then aux k (current_layer+1) (2*layer_end+1) 
          else (layer.(k)<-current_layer;
		aux (k+1) current_layer layer_end)
	in aux 1 1 1 in 
      let unbalanced_events_by_layer = Array.create (layer.(n)+1) [] in
      let unbalanced_events = Array.create (n+1) false in 
      {size=n;
	consistent=true;
	  weigth_of_nodes=t_node;
	  weigth_of_subtrees=t_subtree;
	  unbalanced_events_by_layer=unbalanced_events_by_layer;
	  unbalanced_events=unbalanced_events;
          layer=layer}

    let raz t     = 
      let n = t.size in
      let _ = 
	let rec aux k  =
	    if k=0 then () 
	    else
	      (t.unbalanced_events.(k)<-false ;
	       t.weigth_of_subtrees.(k)<- 0. ;
	       t.weigth_of_nodes.(k)<- 0.)
	in aux n in 
      let _ = 
	Array.iteri 
	  (fun k _ -> t.unbalanced_events_by_layer.(k)<-[])
	  t.unbalanced_events_by_layer
      in
      t
	
    let update_event i w t = 
      let _ = t.weigth_of_nodes.(i)<-w in
      let _ = declare_unbalanced i t in 
      () 
    

    let random t = 
      let t = update_structure t in 
      let a = t.weigth_of_subtrees.(1) in
      if a = 0.0 
      then raise Not_found 
      else
	let r = Random.float a in
	let rec find i r =
	  let node = t.weigth_of_nodes.(i) in
	  if r < node then i 
	  else if 2*i > t.size then raise Not_found 
	  else 
	    let r'=r-.node in
	    let lson = 2*i in
	    let rson = 2*i+1 in
	    let left = t.weigth_of_subtrees.(lson) in
	    if r'<left then find lson r'
	    else 
	      if rson>t.size then raise Not_found 
	      else find rson (r'-.left)
	in 
	let rep = find 1 r in
	rep

 end
