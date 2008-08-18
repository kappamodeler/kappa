(** Functional maps in the fashion of OCaml's Map + extensions + hash consing 
   Some parts proved in Coq *)

(***********************************************************************)
(* Coq extractible code                                                *)
(* David Monniaux, CNRS / Laboratoire d'Informatique de                *)
(*                 l'École Normale Supérieure                          *)
(* Add-ons                                                             *)
(* Bruno Blanchet, CNRS / Laboratoire d'Informatique                   *)
(*                 de l'École Normale Supérieure                       *)
(* Antoine Mine',  Laboratoire d'Informatique de                       *)
(*                 l'École Normale Supérieure                          *)
(* Xavier Rival,   Laboratoire d'Informatique de                       *)
(*                 l'École Normale Supérieure                          *)
(***********************************************************************)

type compare_result =
  | LESS
  | EQUAL
  | GREATER

module type OrderedType = 
  sig
    type t
    type t' 
      (** The type of the map keys. *)
    val compare : t -> t -> int
      (** A total ordering function over the keys.
          This is a two-argument function [f] such that
          [f e1 e2] is zero if the keys [e1] and [e2] are equal,
          [f e1 e2] is strictly negative if [e1] is smaller than [e2],
          and [f e1 e2] is strictly positive if [e1] is greater than [e2].
          Example: a suitable ordering function is
          the generic structural comparison function {!Pervasives.compare}. *)
  end

module Make_sharing(Codomain : OrderedType) =
struct
  type key = Codomain.t
  let compare = Codomain.compare
    
 (* Beginning of code extracted by Coq *)
  type __ = Obj.t

	
 type  'a t =
  | Empty
  | Node of int * 'a cell
 and 'a cell =  'a t * key * 'a * 'a t * int

  let get_key t = 
    match t with Empty -> 0
    | Node(a,_) -> a
  let get_keys (a,b,c,d,e) = (get_key a,b,c,get_key d,e)

  let hash = Hashtbl.create 1
  let fresh = ref 1 
  let hash_tree (a:Codomain.t' cell) =
    let key = get_keys a in 
    try 
      (let rep = Hashtbl.find  hash key in
      rep)
    with
      Not_found -> 
	(let n = !fresh in
	 let new_tree = Node(n,a) in
	Hashtbl.add hash key new_tree ;
(*	 Hashtbl.add hash' n new_tree;*)fresh:=n+1 ;new_tree) 
  let node = hash_tree 

(** val height : 'a1 t -> int **)

let height = function
  | Empty -> 0
  | Node (_,(left, k, data, right, h)) -> h

(** val create : 'a1 t -> key -> 'a1 -> 'a1 t -> 'a1 t **)

let create left key0 data right =
  node (left, key0, data, right, (succ (max (height left) (height right))))

(** val coqfind : 'a1 t -> key -> 'a1 option **)

let rec coqfind tree k =
  match tree with
    | Empty -> None
    | Node (_,(l, v, d, r, height0)) ->
        (match compare k v with
           | cmp when cmp < 0 -> coqfind l k
           | 0 -> Some d
           | _ -> coqfind r k)

(** val bal : 'a1 t -> key -> 'a1 -> 'a1 t -> 'a1 t **)

let bal l k data r =
  let hl = height l in
  let hr = height r in
  (if hl <= (hr + 2)
   then
         (if hr <= (hl + 2)
            then (create l k data r)
            else
                (match r with
                   | Empty -> assert false (* absurd case *)
                   | Node (_,((left, k0, data0, right, height0))) ->
                       (if (height left) <= (height right)
                        then
                              create (create l k data left) k0 data0
                                right
                        else
                              (match left with
                                 | Empty -> assert false
                                     (* absurd case *)
                                 | Node (_,(left0, k1, data1, right0,
                                         height1)) ->
                                     create (create l k data left0) k1
                                       data1
                                       (create right0 k0 data0 right)))))
     else
         (match l with
            | Empty -> assert false (* absurd case *)
            | Node (_,(left, k0, data0, right, height0)) ->
                (if (height right) <= (height left)
                 then
                       create left k0 data0 (create right k data r)
                 else
                       (match right with
                          | Empty -> assert false (* absurd case *)
                          | Node (_,(left0, k1, data1, right0, height1)) ->
                              create (create left k0 data0 left0) k1
                                data1 (create right0 k data r)))))

(** val coqadd : 'a1 t -> key -> 'a1 -> 'a1 t **)

let rec coqadd tree k data =
  match tree with
    | Empty -> node (Empty, k, data, Empty, (succ 0))
    | Node (_,(l, v, d, r, h)) ->
        (match compare k v with
           | cmp when cmp < 0 -> bal (coqadd l k data) v d r
           | 0 -> node (l, k, data, r, h)
           | _ -> bal l v d (coqadd r k data))

(** val min_binding : 'a1 t -> key -> 'a1 -> (key, 'a1) pair **)

let rec min_binding l x d =
  match l with
    | Empty ->  (x, d)
    | Node (_,(l2, x2, d2, right, height0)) -> min_binding l2 x2 d2

(** val remove_min_binding : 'a1 t -> key -> 'a1 -> 'a1 t -> 'a1 t **)

let rec remove_min_binding l x d r =
  match l with
    | Empty -> r
    | Node (_,(l2, x2, d2, r2, height0)) ->
        bal (remove_min_binding l2 x2 d2 r2) x d r

(** val merge : 'a1 t -> 'a1 t -> 'a1 t **)

let merge t1 t2 =
  match t1 with
    | Empty -> t2
    | Node (_,(l1, x1, d1, r1, height0)) ->
        (match t2 with
           | Empty -> t1
           | Node (_,(l2, x2, d2, r2, height1)) ->
               let  (x, d) = min_binding l2 x2 d2 in
               bal t1 x d (remove_min_binding l2 x2 d2 r2))

(** val coqremove : 'a1 t -> key -> 'a1 t **)

let rec coqremove tree x =
  match tree with
    | Empty -> Empty
    | Node (_,(l, v, d, r, h)) ->
        (match compare x v with
           | cmp when cmp < 0 -> bal (coqremove l x) v d r
           | 0 -> merge l r
           | _ -> bal l v d (coqremove r x))

(** val rotate : 'a1 t -> key -> 'a1 t **)

let rec rotate tree v =
  match tree with
    | Empty -> Empty
    | Node (_,(l1, v1, d1, r1, h1)) ->
        (match compare v v1 with
           | cmp when cmp < 0 ->
               (match rotate l1 v with
                  | Empty -> Empty
                  | Node (_,(l2, v2, d2, r2, h2)) -> node (l2, v2, d2,
                      (node (r2, v1, d1, r1, h2)), h1))
           | 0 -> tree
           | _ ->
               (match rotate r1 v with
                  | Empty -> Empty
                  | Node (_,(l2, v2, d2, r2, h2)) -> node ((node (l1, v1,
                      d1, l2, h2)), v2, d2, r2, h1)))

(** val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t **)

let rec map f = function
  | Empty -> Empty
  | Node (_,(l, x, d, r, h)) -> node ((map f l), x, (f d), (map f r), h)

(** val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t **)

let rec mapi f = function
  | Empty -> Empty
  | Node (_,(l, x, d, r, h)) -> node ((mapi f l), x, (f x d), (mapi f r),
      h)

(** val map2inz : (key -> 'a1 -> 'a2 -> 'a3) -> 'a1 t -> 'a2 t -> 'a3 t **)

let rec map2inz f tree1 x =
  match tree1 with
    | Empty -> Empty
    | Node (_,(l1, v1, d1, r1, h1)) ->
        (match rotate x v1 with
           | Empty -> Empty
           | Node (_,(l2, v2, d2, r2, height0)) -> node ((map2inz f l1 l2),
               v1, (f v1 d1 d2), (map2inz f r1 r2), h1))

(** val map2iz : (key -> 'a1 -> 'a1 -> 'a1) -> 'a1 t -> 'a1 t -> 'a1 t **)

let rec map2iz f tree1 x =
  if tree1 == x
  then tree1
  else
        (match tree1 with
           | Empty -> Empty
           | Node (_,(l1, v1, d1, r1, h1)) ->
               (match rotate x v1 with
                  | Empty -> Empty
                  | Node (_,(l2, v2, d2, r2, height0)) -> node
                      ((map2iz f l1 l2), v1, (f v1 d1 d2),
                      (map2iz f r1 r2), h1)))

  (* End of extracted code *)
  let empty = Empty

  let is_empty x = x=Empty

  let add : key -> 'a -> 'a t -> 'a t = fun k v t ->
    coqadd t k v

  let find : key -> 'a t -> 'a = fun v t ->
    match (coqfind t v) with
      None -> raise Not_found
    | Some x -> x

  let mem : key -> 'a t -> bool = fun v t ->
    match (coqfind t v) with
      None -> false
    | Some _ -> true

  let remove : key -> 'a t -> 'a t = fun k v ->
    coqremove v k

  let rec iter f = function
      Empty -> ()
    | Node(_,(l, v, d, r, _)) ->
        iter f l; f v d; iter f r

  let rec fold f node rest =
    match node with
      Empty -> rest
    | Node(_,(l, v, d, r, _)) ->
	fold f l (f v d (fold f r rest))

	  (*** Map2 Supplemental code ***)
	  (** Internals **)
  let rec join l x d r =
    match bal l x d r with
      Empty -> invalid_arg "Map2.join"
    | Node(_,(l', x', d', r', _)) as t' ->
        let h = height l' - height r' in
        if h < -2 || h > 2 then join l' x' d' r' else t'

	  (* utility function that cuts m into the keys smaller than v, the data
	     associated to v, and the keys greater than v *)
  let rec cut v = function
      Empty -> raise Not_found
    | Node (_,(l1,v1,d1,r1,h1)) ->
        let c = Codomain.compare v v1 in
	if c <0 then
          let (l2,d2,r2) = cut v l1 in (l2,d2,node (r2,v1,d1,r1,h1))
        else if c > 0 then
          let (l2,d2,r2) = cut v r1 in (node (l1,v1,d1,l2,h1),d2,r2)
        else (l1,d1,r1)

	    (* the same as cut, does not fail if key v is not found in m and always
	       return the keys smaller than v, and the keys greater than v *)
  let rec cut_opt v = function
      Empty -> Empty,None,Empty
    | Node (_,(l1,v1,d1,r1,h1)) ->
        let c = Codomain.compare v v1 in
        if c < 0 then 
          let (l2,d2,r2) = cut_opt v l1 in (l2,d2,node (r2,v1,d1,r1,h1))
        else if c > 0 then
          let (l2,d2,r2) = cut_opt v r1 in (node (l1,v1,d1,l2,h1),d2,r2)
        else (l1,Some d1,r1)

	    (* as cut_opt, but returns balanced trees *)
  let rec split v = function
      Empty -> Empty,None,Empty
    | Node (_,(l1,v1,d1,r1,_)) ->
        let c = Codomain.compare v v1 in
        if c<0 then 
          let (l2,d2,r2) = split v l1 in (l2,d2,join r2 v1 d1 r1)
        else if c>0 then
          let (l2,d2,r2) = split v r1 in (join l1 v1 d1 l2,d2,r2)
        else (l1,Some d1,r1)

	    (* Same as merge, but does not assume anything about l and r. *)

  let rec concat t1 t2 =
    match (t1, t2) with
      (Empty, t) -> t
    | (t, Empty) -> t
    | (Node(_,(l1, v1, d1, r1, h1)), Node(_,(l2, v2, d2, r2, h2))) ->
        join l1 v1 d1 (join (concat r1 l2) v2 d2 r2)

	  (*** Map2 ***)              

  let rec map2i f1 f2 f m1 m2 =
    if m1==m2 then m1 else
    match m1 with
      Empty -> mapi f2 m2
    | Node (_,(l1,v1,d1,r1,h1)) ->
        let (l2,dd2,r2) = split v1 m2 in
        match dd2 with
          Some d2 -> 
            let dd = if d1==d2 then d1 else f v1 d1 d2 in
            join (map2i f1 f2 f l1 l2) v1 dd (map2i f1 f2 f r1 r2)
        | None -> 
            join (map2i f1 f2 f l1 l2) v1 (f1 v1 d1) (map2i f1 f2 f r1 r2)

  let rec mapio f = function
      Empty               -> Empty
    | Node(_,(l, v, d, r, h)) -> 
        match f v d with
          None    -> Empty
        | Some dd -> join (mapio f l) v dd (mapio f r)

	      (* as map2i, but f, f1, f2 can return None and the element key will not be
		 associated in the result. *)
  let rec map2io f1 f2 f m1 m2 =
    if m1==m2 then m1 else
    match m1 with
      Empty -> mapio f2 m2
    | Node (_,(l1,v1,d1,r1,_)) ->
        let (l2,dd2,r2) = split v1 m2 in
        let dd = match dd2 with None -> f1 v1 d1 | Some d2 -> 
          if d1==d2 then Some d1 else f v1 d1 d2 in
        match dd with
          None -> concat (map2io f1 f2 f l1 l2) (map2io f1 f2 f r1 r2)
        | Some dd -> join (map2io f1 f2 f l1 l2) v1 dd (map2io f1 f2 f r1 r2)

	      (* also apply f when the two values are the same *)
  let rec map2 f1 f2 f m1 m2 =
    match m1 with
      Empty -> mapi f2 m2
    | Node (_,(l1,v1,d1,r1,h1)) ->
        let (l2,dd2,r2) = split v1 m2 in
        match dd2 with
          Some d2 -> 
            let dd = f v1 d1 d2 in
            join (map2 f1 f2 f l1 l2) v1 dd (map2 f1 f2 f r1 r2)
        | None -> 
            join (map2 f1 f2 f l1 l2) v1 (f1 v1 d1) (map2 f1 f2 f r1 r2)

	      (*** Forall2 ***) 
  let rec forall2iz p m1 m2 =
    if m1==m2 then true else
    match m1 with
      Empty -> if m2=Empty then true 
      else raise (Invalid_argument "Map2.forall2iz")
    | Node (_,(l1,v1,d1,r1,h1)) ->
        let (l2,d2,r2) = try cut v1 m2 
	with Not_found -> raise (Invalid_argument "Map2.forall2iz") in
        (if d1==d2 then true else p v1 d1 d2)
          && (forall2iz p l1 l2)
	  && (forall2iz p r1 r2)

	  (*** Iter2 ***)
  let rec iter2iz f m1 m2 =
    if m1==m2 then () else
    match m1 with
      Empty -> if m2=Empty then () 
      else raise (Invalid_argument "Map2.iter2iz")
    | Node (_,(l1,v1,d1,r1,h1)) ->
        let (l2,d2,r2) = try cut v1 m2 
	with Not_found -> raise (Invalid_argument "Map2.iter2iz")
	in
        if d1!=d2 then f v1 d1 d2;
        iter2iz f l1 l2;
	iter2iz f r1 r2
	  
	  (* unlike iter2iz, this always suceeds: f1 (resp. f2) is called on keys 
	     that are defined only in m1 (resp. m2). *)
  let rec iter2i f1 f2 f m1 m2 =
    if m1==m2 then () else
    match m1 with
      Empty -> iter f2 m2
    | Node (_,(l1,v1,d1,r1,h1)) ->
	let (l2,d2,r2) = cut_opt v1 m2 in
	(match d2 with
	  Some dd2 -> if d1!=dd2 then f v1 d1 dd2
	| None -> f1 v1 d1);
	iter2i f1 f2 f l1 l2;
	iter2i f1 f2 f r1 r2

  let rec iter2 f1 f2 f m1 m2 =
    match m1 with
      Empty -> iter f2 m2
    | Node (_,(l1,v1,d1,r1,h1)) ->
	let (l2,d2,r2) = cut_opt v1 m2 in
	(match d2 with
	  Some dd2 -> if d1!=dd2 then f v1 d1 dd2
	| None -> f1 v1 d1);
	iter2 f1 f2 f l1 l2;
	iter2 f1 f2 f r1 r2

  let rec size = function
    | Empty -> 0
    | Node (_,(m0,_,_,m1,_)) -> (size m0) + (size m1) + 1


	  (*** Fold2 ***)
  let fold_two_failed f m0 m1 x =
    fold
      (fun i x0 nx ->
	if mem i m1 then
	  let x1 = find i m1 in f i x0 x1 x
	else raise (Invalid_argument "fold_two_failed")
      ) m0 x
  let rec fold_two f m0 m1 x =
    match m0,m1 with
    | Empty,Empty -> x
    | Node (_,(m00,i0,x0,m01,h0)),Node (_,(m10,i1,x1,m11,h1)) ->
	if i0 = i1 && h0 = h1 then
	  fold_two f m00 m10 (fold_two f m01 m11 (f i0 x0 x1 x))
	else fold_two_failed f m0 m1 x
    | _,_ -> fold_two_failed f m0 m1 x
  let fold2i = fold_two
      (* also apply f when the two values are the same *)
  let rec fold2 f1 f2 f m1 m2 z0 =
    match m1 with
      Empty -> fold f2 m2 z0
    | Node (_,(l1,v1,d1,r1,h1)) ->
        let (l2,dd2,r2) = split v1 m2 in
        match dd2 with
          Some d2 -> 
	    (fold2 f1 f2 f l1 l2
	       (f v1 d1 d2
		  (fold2 f1 f2 f r1 r2 z0)))
        | None -> 
	    (fold2 f1 f2 f l1 l2
	       (f1 v1 d1
		  (fold2 f1 f2 f r1 r2 z0)))


  let iter_two_failed f m0 m1 =
    iter
      (fun i x0 ->
	if mem i m1 then
	  let x1 = find i m1 in f i x0 x1
	else raise (Invalid_argument "iter_two_failed")
      ) m0

  let rec iter_two f m0 m1 =
    match m0,m1 with
    | Empty,Empty -> ()
    | Node (_,(m00,i0,x0,m01,h0)),Node (_,(m10,i1,x1,m11,h1)) ->
	if i0 = i1 && h0 = h1 then
	  begin
	    iter_two f m00 m10;
	    f i0 x0 x1;
	    iter_two f m01 m11;
	  end
	else iter_two_failed f m0 m1
    | _,_ -> iter_two_failed f m0 m1


  type 'a delta_record = 
      ('a t) *   (* keys in m2, possibly not in m1 *)
      (key list) (* keys in m1 and not in m2 *)

  let compute_delta (equal:'a->'a->bool) (m1:'a t) (m2:'a t) =
    let to_add = ref empty
    and to_rem = ref [] in
    iter2i 
      (fun k v1 -> to_rem := k::(!to_rem))
      (fun k v2 -> to_add := add k v2 !to_add)
      (fun k v1 v2 ->
	if not (equal v1 v2) then to_add := add k v2 !to_add)
      m1 m2;
    !to_add, !to_rem

  let apply_delta (m1:'a t) ((add,rem):'a delta_record) =
    let m2 = 
      map2i
	(fun k v -> v)
	(fun k v -> v)
	(fun k v1 v2 -> v2)
	m1 add
    in 
    List.fold_left (fun m k -> remove k m) m2 rem

  (*let tree_of_int i = i 
   (* Hashtbl.find hash' i *)
  let int_of_tree t = t*)
     
end
