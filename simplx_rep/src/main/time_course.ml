open Simulation2
open Data
open Mods2
open Rule

let output_data_point desc_opt sd p c = 
  let rec print_values d k last_k time obs_list = 
    if k=last_k then () 
    else
      let time = if !time_mode then (!time_sample *. (float_of_int last_k)) +. !init_time else time in
	Printf.fprintf d "%E\t%s\n" time (String.concat "," obs_list) ;
	print_values d k (last_k+1) time obs_list
  in
    match !desc_opt with
	None -> c
      | Some d ->
	  match c.points with
	      [] -> c
	    | (k,time,obs_list)::_ ->
		begin
		  if (k = c.last_k) or (c.last_k = -1) then c
		  else
		    let _ = 
		      if k = 0 then
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
			  Printf.fprintf d "#t\t%s" (String.concat "\t" entete) ;
			  Printf.fprintf d "\n" 
		    in
		      print_values d k (c.last_k) time obs_list ;
		      {c with last_k = k}
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
