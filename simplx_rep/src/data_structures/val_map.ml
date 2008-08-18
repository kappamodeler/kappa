open Random_tree 

module type OrderedType =
  sig
    type t
    type map_val
    val def:map_val
    val compare: t -> t -> int
    val to_f: map_val -> float
  end

module type S =
  sig
    type key
    type t
    type val_type
    val empty: int -> t
    val copy: t->t
    val copy_in: t->t->t
      (*val is_empty: t -> bool*)
    val add: key -> val_type -> t -> t
    val find: key -> t -> val_type
    val restore_consistency: t -> t
    (*val remove: key -> t -> t*)
      (*val mem:  key -> t -> bool*)
    val iter: (key -> val_type -> unit) -> t -> unit
    val fold: (key -> val_type -> 'a -> 'a) -> t -> 'a -> 'a
      (*val compare: (val_type -> val_type -> int) -> t -> t -> int
	val equal: (val_type -> val_type -> bool) -> t -> t -> bool*)
      (*val random: t -> key * val_type
	val size: t -> int*)
    val random_val: t -> key * val_type
    val accval: t -> float
  end

module Make(Ord:OrderedType with type t = int) = 
  (struct

    type key = Ord.t
    type val_type = Ord.map_val

	  
    type t =
        Empty
      | Node of t * key * Ord.map_val * t * int * int * float (*Node(left,key,value,right,height,size,acc)*)
	  
    let copy t = t
    let copy_in t1 t2 = t1
    let height = function
        Empty -> 0
      | Node(_,_,_,_,h,_,_) -> h

    let size = function
        Empty -> 0
      | Node(_,_,_,_,_,s,_) -> s

    let accval = function
	Empty -> 0.0
      | Node(_,_,_,_,_,_,acc) -> acc
	  
    let create l x d r =
      let hl = height l and hr = height r in
      let acc1 = accval l and acc2 = accval r in
      Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1), (size l) + (size r) + 1, (Ord.to_f d) +. acc1 +. acc2)

    let bal l x d r =
      let hl = match l with Empty -> 0 | Node(_,_,_,_,h,_,_) -> h in
      let hr = match r with Empty -> 0 | Node(_,_,_,_,h,_,_) -> h in
      if hl > hr + 2 then begin
        match l with
          Empty -> invalid_arg "Val_map.bal"
        | Node(ll, lv, ld, lr, _,_,_) ->
            if height ll >= height lr then
              create ll lv ld (create lr x d r)
            else begin
              match lr with
                Empty -> invalid_arg "Val_map.bal"
              | Node(lrl, lrv, lrd, lrr, _,_,_)->
                  create (create ll lv ld lrl) lrv lrd (create lrr x d r)
            end
      end else if hr > hl + 2 then begin
        match r with
          Empty -> invalid_arg "Val_map.bal"
        | Node(rl, rv, rd, rr, _,_,_) ->
            if height rr >= height rl then
              create (create l x d rl) rv rd rr
            else begin
              match rl with
                Empty -> invalid_arg "Val_map.bal"
              | Node(rll, rlv, rld, rlr, _,_,_) ->
                  create (create l x d rll) rlv rld (create rlr rv rd rr)
            end
      end else
	let acc1 = accval l and acc2 = accval r in
          Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1), (size l) + (size r) + 1, (Ord.to_f d) +. acc1 +. acc2)

    let empty n  = Empty
    let restore_consistency t = t
    let is_empty = function Empty -> true | _ -> false

    let rec add x data = function
        Empty -> Node(Empty, x, data, Empty,1,1,Ord.to_f data)
      | Node(l, v, d, r, h,s,acc) ->
          let c = Ord.compare x v in
          if c = 0 then
            Node(l, x, data, r, h,s,acc -. (Ord.to_f d) +. (Ord.to_f data))
          else if c < 0 then
            bal (add x data l) v d r
          else
            bal l v d (add x data r)

    let rec find x = function
        Empty ->
          raise Not_found
      | Node(l, v, d, r, _,_,_) ->
          let c = Ord.compare x v in
          if c = 0 then d
          else find x (if c < 0 then l else r)

    let rec find_acc aim_acc = function
	Empty -> raise Not_found
      | Node(l,key,d,r,_,_,acc) -> 
	  if aim_acc >= acc then invalid_arg "Val_map.find_acc" (*invariant*)
	  else 
	    let acc_l = accval l and acc_r = accval r in
	      if acc_l > aim_acc then find_acc aim_acc l
	      else 
		if (acc_r +. acc_l) > aim_acc then find_acc (aim_acc -. acc_l) r
		else (key,d)  

    let rec mem x = function
        Empty ->
          false
      | Node(l, v, d, r, _,_,_) ->
          let c = Ord.compare x v in
          c = 0 || mem x (if c < 0 then l else r)

    let rec min_binding = function
        Empty -> raise Not_found
      | Node(Empty, x, d, r, _,_,_) -> (x, d)
      | Node(l, x, d, r, _,_,_) -> min_binding l

    let rec remove_min_binding = function
        Empty -> invalid_arg "Val_map.remove_min_elt"
      | Node(Empty, x, d, r, _,_,_) -> r
      | Node(l, x, d, r, _,_,_) -> bal (remove_min_binding l) x d r

    let merge t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) ->
          let (x, d) = min_binding t2 in
          bal t1 x d (remove_min_binding t2)

    let rec remove x = function
        Empty ->
          Empty
      | Node(l, v, d, r, _,_,_) ->
          let c = Ord.compare x v in
          if c = 0 then
            merge l r
          else if c < 0 then
            bal (remove x l) v d r
          else
            bal l v d (remove x r)

    let rec iter f = function
        Empty -> ()
      | Node(l, v, d, r, _,_,_) ->
          iter f l; f v d; iter f r

    let rec map f = function
        Empty               -> Empty
      | Node(l, v, d, r, h,s,acc) -> Node(map f l, v, f d, map f r, h,s,acc)

    let rec mapi f = function
        Empty               -> Empty
      | Node(l, v, d, r, h,s,acc) -> Node(mapi f l, v, f v d, mapi f r, h,s,acc)

    let rec fold f m accu =
      match m with
        Empty -> accu
      | Node(l, v, d, r, _,_,_) ->
          fold f r (f v d (fold f l accu))

    type enumeration = End | More of key * Ord.map_val * t * enumeration

    let rec cons_enum m e =
      match m with
        Empty -> e
      | Node(l, v, d, r, _,_,_) -> cons_enum l (More(v, d, r, e))

    let compare cmp m1 m2 =
      let rec compare_aux e1 e2 =
          match (e1, e2) with
          (End, End) -> 0
        | (End, _)  -> -1
        | (_, End) -> 1
        | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
            let c = Ord.compare v1 v2 in
            if c <> 0 then c else
            let c = cmp d1 d2 in
            if c <> 0 then c else
            compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
      in compare_aux (cons_enum m1 End) (cons_enum m2 End)

    let equal cmp m1 m2 =
      let rec equal_aux e1 e2 =
          match (e1, e2) with
          (End, End) -> true
        | (End, _)  -> false
        | (_, End) -> false
        | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
            Ord.compare v1 v2 = 0 && cmp d1 d2 &&
            equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
      in equal_aux (cons_enum m1 End) (cons_enum m2 End)


    let random (m:t) = 
      let s = size m in 
	if s=0 then raise Not_found
	else 
	  let rec find k m = 
	    match m with 
		Empty -> invalid_arg "Val_map.random"
	    | Node (l,key,v,r,_,_,_) -> 
	  	  if k=0 then (key,v)
		  else 
		    let s = size l in 
		      if k<=s then find (k-1) l
		      else find (k-s-1) r
	  in
	    find (Random.int (size m)) m


    let random_val (m:t) = 
      match accval m with 
	  0.0 -> raise Not_found
	| f ->
	    
	    try 
	      (
	      let r = Random.float f in
	      let rep = find_acc (Random.float r) m in
	      rep)
	    with Invalid_argument "Val_map.find_acc" -> invalid_arg "Val_map.random_val"

end:S with type val_type = Ord.map_val and type key = Ord.t  )


module Make_in_place(Ord:OrderedType with type t = int) = 
  (struct

    type key = Ord.t

    type val_type = Ord.map_val

    type t = {random:Random_tree.tree;
              asso:val_type array}

    let update_structure  t = 
      let _ = Random_tree.update_structure t.random in
      t
    let restore_consistency = update_structure 

    let copy t = {random = Random_tree.copy t.random ; 
		  asso = Array.copy t.asso} 
   
    let copy_in t1 t2  = 
      let _ = Random_tree.copy_in t1.random t2.random in
      let _ = Array.iteri (fun a b -> t2.asso.(a)<-b) t1.asso in
	t2

    let accval t = 
      let t = update_structure t in
      t.random.Random_tree.weigth_of_subtrees.(1) 

    let empty n  = 
      {asso=Array.make (n+1) Ord.def;
       random=Random_tree.create n} 

    let add a b t =
      let _ = Random_tree.update_event a (Ord.to_f b) t.random in
      let _ = t.asso.(a) <- b 
      in t 
  
    let random_val t = 
      let a= Random_tree.random t.random in a,t.asso.(a)

    let find a t = t.asso.(a) 

    let fold f t  = 
      let rec aux k rep = 
	if k>t.random.Random_tree.size  then rep 
	else 
	  aux 
	    (k+1) 
	    (f k (t.asso.(k)) rep)
      in aux 1 

    let iter f t = 
      let g a b c = f a b in
      fold g t ()
end:S with type val_type = Ord.map_val and type key = Ord.t  )

 

