open Data_structures_metaplx 

let id = (fun x -> x)

let macro_expanse l = 
  let rec scan f l sol = 
    match l with 
      [] -> List.rev sol 
    | t::q -> 
	begin
	  let t' = 
	    match t with 
	      PP_INIT_L (x,i) -> INIT_L(let a,b = x f in a,b,i)
	    | PP_DONT_CARE_L (x,i) -> DONT_CARE_L(x f,i)
	    | PP_OBS_L (x,i) -> OBS_L(let a,b = x f in a,b,i)
	    | PP_STORY_L (x,i) -> STORY_L(let a,b = x f in a,b,i)
	    | PP_GEN_L (x,i) -> GEN_L(x f,i)
	    | PP_CONC_L (x,i) -> CONC_L(x f,i)
	    | PP_RULE_L (x,i) -> RULE_L(x f,i)
	    | PP_PREPROCESSED_RULE (x,i) -> PREPROCESSED_RULE(let a,b = x f in a,b,i)
	  in scan f q (t'::sol) 
	end
  in scan id l [] 
