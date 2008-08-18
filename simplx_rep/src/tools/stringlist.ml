module type StringList = 
    sig
      type t 
      val empty: t
      val dump: out_channel -> t -> unit
      val concat: t -> t -> t
      val push: string -> t -> t
      val build: string list -> t
      val build_between: string -> string list -> t
      val build_before: string list -> t -> t
      val build_between_before: string -> string list -> t -> t
      val concatlist: string -> t list -> t
    end

module StringList =
  (struct 
    type t = string list 
    let empty = []
    let dump log = List.iter (Printf.fprintf log "%s")
    let concat a b = a@b
    let push a b = a::b 
    let build a = a 
    let build_before a b = a@b 
    let build_between a l = 
      fst (List.fold_left 
	(fun (sol,bool) k -> 
	  if bool then (k::a::sol,true)
	  else (k::sol,false))
	([],false) (List.rev l))
    let build_between_before a l sufix = 
       fst (List.fold_left 
	(fun (sol,bool) k -> 
	  if bool then (k::a::sol,true)
	  else (k::sol,false))
	(sufix,false) (List.rev l))
 
   let concatlist a b = 
      fst (List.fold_left 
	(fun (sol,bool) k -> 
	  if bool then build_between_before a k (push a sol),true 
	  else build_between_before a k sol,true
	      )
	([],false) (List.rev b))
      
	end:StringList)

  
module SingleString = 
  (struct 
    type t = string
    let empty = ""
    let dump log x = Printf.fprintf log "%s" x
    let concat a b = a^b
    let push = concat 
    let build = String.concat ""
    let build_before a b = (build a)^b
    let build_between = String.concat
    let build_between_before a b c = (build_between a b)^c
    let concatlist a b = 
      String.concat a (List.fold_left (fun a b -> b::a) [] (List.rev b))
  end:StringList)
