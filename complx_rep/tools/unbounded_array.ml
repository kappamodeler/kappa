module ArrayExt = 
  struct
    type 'a t = 
	{mutable size:int;
         mutable array:'a option array;
         mutable max_elt:int}

    let create a = 
      {size = 0;
       max_elt = 0;
       array = Array.create 0 (None:'a option)} 
 
    
    let find x a = 
      if x<1 or x>a.size then raise Not_found 
      else
	match a.array.(x-1) with 
	  None -> raise Not_found
	| Some a -> a
	 
    let direct_find x a = a.array.(x-1)

    let fold f x sol = 
      let g k y x = 
	match y with None -> x
	| Some a -> f k a x in 
      let n = x.size in 
      let rec aux k sol = 
	if k>n then sol  
	else
	  aux 
	    (k+1) 
	    (g k (direct_find k x ) sol)
      in aux 1 sol 

    let double a =
      let n = a.size in 
      let n' = max 1 (2*n) in 
      let array' = Array.create (n'-n) None in
      let _ = a.size <- n' in
      let _ = a.array <- (Array.concat [a.array;array']) in 
      a

    let rec add k x a = 
      if k>a.size then 
	add k  x (double a)
      else
	let _ = (a.array.(k-1)<-Some x;
                 if k > a.max_elt then a.max_elt<- k)
	in a
      
    let length a = a.max_elt

    let iter f x = 
       let g k y  = 
	match y with None -> ()
	| Some a -> f k a  in 
       let n = x.size in 
       let rec aux k  = 
	 if k>n then ()
	 else
	   let _ = g k (direct_find k x) in 
	   aux (k+1)
       in aux 1  

    let rec fold2 f g h x y sol = 
      let n = x.size in
      let n' = y.size in
      if n<n' then
	let rec aux k sol = 
	    if k>n then sol
	    else
	      match direct_find k x,direct_find k y  
	      with 
		None,None -> aux (k+1) sol 
	      |	Some a,Some b -> aux (k+1) (h k a b sol) 
	      |	None,Some b-> aux (k+1) (g k b sol)
	      |	Some a,None -> aux (k+1) (f k a sol)
	in 
	let sol  = aux 1 sol  in 
	let rec aux k sol = 
	  if k>n' then sol  
	  else 
	    match direct_find k y with 
	      None -> aux (k+1) sol 
	    | Some a -> aux (k+1) (g k a sol ) 
	in 
	aux (n+1) sol 
      else
	let rec aux k sol = 
	    if k>n' then sol
	    else
	      match direct_find k x,direct_find k y  
	      with 
		None,None -> aux (k+1) sol 
	      |	Some a,Some b -> aux (k+1) (h k a b sol) 
	      |	None,Some b-> aux (k+1) (g k b sol)
	      |	Some a,None -> aux (k+1) (f k a sol)
	in 
	let sol  = aux 1 sol  in 
	let rec aux k sol = 
	  if k>n then sol  
	  else 
	    match direct_find k x with 
	      None -> aux (k+1) sol 
	    | Some a -> aux (k+1) (f k a sol ) 
	in 
	aux (n'+1) sol 


    let map f x  = 
      let f x = 
	match x with None -> None  
	| Some a -> Some (f a) 
      in
	{array = Array.map f x.array;
	 max_elt = x.max_elt;
	 size =x.size}


    let rec map2 f g h x y = 
      let n = x.size in
      let n' = y.size in
      if n<n' then map2 g f (fun k x y -> h k y x) y x 
      else
	let rec aux k = 
	    if k>n' then ()
	    else
	      match direct_find k x,direct_find k y  
	      with 
		None,None -> aux (k+1)
	      |	Some a,Some b ->
		  let _ = add k (h k a b) x in aux (k+1)
	      |	None,Some b->
		  let _ = add k (g k b) x in aux (k+1)
	      |	Some a,None -> 
		  let _ = add k (f k a) x in aux (k+1)
	in 
	let _ = aux 1 in 
	let rec aux k = 
	  if k>n then () 
	  else 
	    match direct_find k x with 
	      None -> aux (k+1)
	    | Some a -> let _ = add k (f k a) x in aux (k+1)
	in 
	let _ = aux (n'+1) in
	x
	
	  
  end
