open Data_structures_metaplx 

let compatible b d = 
  let rec vide l1 l2 = 
    match l1,l2 with 
      (a,_)::b,(c,_)::d when a=c -> vide b d
    | [],[] -> true
    | _ -> false 
  in vide b d 

let smash b d = 
  let rec vide l1 l2 rep = 
    match l1,l2 with 
       (a,x)::b,(c,y)::d when a=c -> vide b d ((a,(x,y))::rep)
    | [],[] -> List.rev rep
    | _ -> failwith "smash"
  in vide b d []


let convert lines = 
    (fun x -> 
      match x with 
	RULE_L (x,i) -> 
	  begin
	    let flag,((lhs,lhs_annotation),sign,(rhs,rhs_annotation),rule_annotation1,rule_annotation2) = x in 
	    let common,left,right = 
	      let rec vide (a,b) c = 
		match a,b with [],[] -> List.rev c,[],[]
		| [],b -> List.rev c,[],b
		| a,[] -> List.rev c,a,[]
		| (a,b)::q,(e,d)::q' when a=e && compatible b d -> 
		    vide (q,q') ((a,smash b d)::c)
		| _ -> List.rev c,a,b
	      in 
	      vide (lhs,rhs) [] in 
	    let g  = 
	      List.map 
		(fun (a,b) -> 
		  {agent_name=a;
		    interface =b}) in 
	     let h  = 
	      List.map 
		(fun (a,b) -> 
		  {agent_name=a;
		    interface =b}) in 
	    let rule =  {flag=flag;
			  hand_side_common=g common;
			  mod_left_hand_side=
			  if sign ="->" 
			  then 
			    h 
			      left 
			  else [];
			  mod_right_hand_side=[];
			  fixed_left_hand_side=if sign="->" then [] else h left;
			  fixed_right_hand_side=h right;
			  sign=sign;
			  lhs_annotation=lhs_annotation;
			  rhs_annotation=rhs_annotation;
			  rule_annotation=rule_annotation1^rule_annotation2} 
	    in 
	    PREPROCESSED_RULE (x,rule,i)
	  end
      |	_ -> x)
    lines 
