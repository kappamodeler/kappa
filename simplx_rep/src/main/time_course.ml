open Simulation2
open Data
open Mods2
open Rule

let rec find_val t c obs =
  if t < 0 then Error.runtime (None,None,None) ("Observable "^(string_of_int obs)^" not defined!")
  else
    let obs_map = try IntMap.find t c.concentrations with Not_found -> IntMap.empty in
    let opt = try Some (IntMap.find obs obs_map) with Not_found -> None in
      match opt with
	  Some v -> v
	| None -> find_val (t-1) c obs
		

let output_data_point desc_opt sd p c = 
  match !desc_opt with
      None -> c
    | Some d ->
	
	let t = 
	  if !time_mode then get_time_range c.curr_time (*get the time interval corresponding to current time*)
	  else get_step_range c.curr_step (*get the event interval corresponding to current event*)
	in
	  if t = c.last_measure then c
	  else
	    begin
	      if c.last_measure = (-1) then (*if first measure*)
		begin
		  let entete = 
		    IntSet.fold (fun i cont -> 
				   let r,_ = 
				     try Rule_of_int.find i sd.rules 
				     with Not_found -> 
				       let s = "Main.output_data_point" in
					 Error.runtime (None,None,None) s
				   in
				     if r.input = "var" then cont
				     else
				       let s = 
					 match r.flag with 
					     Some flg -> flg 
					   | None -> 
					       let s = "Main.output_data_point" in
						 Error.runtime (None,None,None) s
				       in
					 s::cont
				) sd.obs_ind []
		  in
		    Printf.fprintf d "#t\t%s" (String.concat "\t" entete)
		end ;
	      Printf.fprintf d "\n" ;
	      let line =
		IntSet.fold (fun i cont-> 
			       let r,_ = Rule_of_int.find i sd.rules in
				 if r.input = "var" then cont
				 else
			       	   (Printf.sprintf "%d" (int_of_float (find_val t c i)))::cont
			    ) sd.obs_ind [] 
	      in
		Printf.fprintf d "%s" (String.concat "\t" ((Printf.sprintf "%.3E" c.curr_time)::line));
		{c with last_measure = t}
	    end

let make_gnuplot_file data_file sd = 
  let plot_file = ((chop_extension data_file)^".gplot") in
  let d = open_out plot_file in
    Printf.fprintf d "set xlabel 'Time'\n" ;
    Printf.fprintf d "set ylabel 'Number/Activity'\n" ;
    Printf.fprintf d "set autoscale\n" ;
    let args = 
      let rescale_str = 
	if (!rescale < 1.0) or (!rescale > 1.0)
	then [Printf.sprintf "rescale=%.4f" !rescale] else [] in
      let sample_str = 
	if !time_mode then [Printf.sprintf "sample=%.4ft.u" !time_sample]
	else [Printf.sprintf "sample=%d event(s)" !step_sample]
      in
      let add_str = [!fic] in
	String.concat " " (add_str@rescale_str@sample_str)
    in
    let date = 
      let tm = Unix.localtime (Unix.time ()) 
      in
	Printf.sprintf "%d/%d/%d" 
	  tm.Unix.tm_mday tm.Unix.tm_mon (tm.Unix.tm_year + 1900) 
    in
      Printf.fprintf d "set title '%s %s'\n" date args ;
      let entete = IntSet.fold (fun i cont ->
				  let r,_ = Rule_of_int.find i sd.rules in
				    if r.input = "var" then cont else ((Rule.name r)::cont)
			       ) sd.obs_ind []
      in
      let _ = 
	List.fold_left (fun c title ->
			  let style = 
			    if String.contains title '[' then "w l"
			    else "w p"
			  in
			    if c = 2 then (
			      Printf.fprintf d "plot '%s' using 1:%d title '%s' %s\n" data_file c title style;
			      c+1
			    )
			    else (
			      Printf.fprintf d "replot '%s' using 1:%d title '%s' %s\n" data_file c title style;
			      c+1
			    )
		       ) 2 entete
      in
        close_out d ;
	let _ = Sys.command (Printf.sprintf "gnuplot -persist %s" plot_file) in ()
