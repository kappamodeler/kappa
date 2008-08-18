(* 17/10/2006 *)
(* Static analysis of BNG systems*)
(* Jerome Feret pour PlectiX *)
(* Hash tables*)
(* hash.ml *)
  
open Data_structures
open Pb_sig
open Hashtbl 
  
  
module HInit = 
  functor (M:Map.S) -> 
  struct 
    let create = Hashtbl.create 
    let add = Hashtbl.add
    let find = Hashtbl.find
  end
      
module HT = 
  functor (M:Map.S) -> 
  struct 
    let enlarge_constant = 8
	
    let create n  = 
      let n = max 1 n in 
      let t = ref (Array.create n None)in 
      let l = ref [] in 
      let occu = ref 0 in 
      let size = ref n in 
      let rec double () =
	(let new_size = (!size)*enlarge_constant in 
	t:= Array.create new_size None;
	size:=new_size)
      and copy u =
	let old = (!l) in 
	(l:=[];
	 let rec aux l = 
	   match l with k::q -> 
	     (match u.(k) with None -> () | Some a -> add_really a);
	     aux q
	   |	[] -> ()
	 in aux old)
      and add k im = 
	((if (!occu)>=(!size)/enlarge_constant then 
	  let u=(!t) in 
	  (double ();copy u));add_really (k,Hashtbl.hash k,im))
      and add_really ((k:'a),h,(im:'b)) = 
	let rec aux i = 
	  let h' = (i+1) mod (!size) in 
	  (if h'<0 or h'>=(!size) then (print_int h';print_newline ()));
	  match (!t).(h') with None -> (occu:=(!occu)+1;l:=(h')::(!l);
					(!t).(h')<- Some ((k:'a),(h:int),(im:'b)))
	  |	Some (a,b,c) when a=k -> (!t).(h)<- Some (k,h,im)
	  |	Some _ -> aux (i+1) in
	aux h 
      and find k = 
    let h=Hashtbl.hash k in 
    let rec aux i = 
      let h = (i+1) mod (!size) in 
      match (!t).(h) with None -> raise Not_found 
      |	Some (a,b,c) when a=k -> c
      |	Some _ -> aux (i+1) 
    in aux h in 
      (add,find) 
	
    let add (f,g)  k im  = f k im
    let find (f,g) k  = g k 
  end
      
module HTL = 
  functor (M:Map.S) -> 
  struct 
    let enlarge_constant = 4
	
    let create n  = 
      let n = max 1 n in 
      let t = ref (Array.create n [])in 
      let l = ref [] in 
      let occu = ref 0 in 
      let size = ref n in 
      let rec double () =
	(let new_size = (!size)*enlarge_constant in 
	t:= Array.create new_size [];
	size:=new_size)
      and copy u =
	let old = (!l) in 
	(l:=[];
	 let rec aux l = 
	   match l with k::q -> (List.iter add_really (u.(k));aux q)
	   |	[] -> ()
	 in aux old)
      and add k im = 
	((if (!occu)>=(!size)/enlarge_constant then 
	  let u=(!t) in 
	  (double ();copy u));add_really (k,Hashtbl.hash k,im))
      and add_really ((k:'a),h,(im:'b)) = 
	let h'=h mod (!size) in 
	let tab = (!t) in 
	(tab.(h') <- (k,h,im)::(tab.(h'));
	 occu:=1+(!occu))
      and find k = 
	let h=Hashtbl.hash k in 
	let h'=h mod (!size) in 
	let rec aux l = 
	  match l with 
	    (a,b,c)::q when a=k -> c
	  | _::q -> aux q
	  | [] -> raise Not_found
	in aux (!t).(h') in 
      (add,find) 
	
    let add (f,g)  k im  = f k im
    let find (f,g) k  = g k 
  end

module HM = 
  functor (M:Map.S) -> 
    struct 
      let create n  = 
	let h = ref M.empty in 
	let add k im = h:=M.add k im (!h) in 
	let find k   = M.find k (!h) 
      in (add,find)
      let add (f,g)  k im  = f k im
      let find (f,g) k  = g k 	
    end

