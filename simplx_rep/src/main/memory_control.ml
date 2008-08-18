open Data
open Printf

let smaller a b = a<b
let greater a b = a>b

let test limit comp =
  if limit < 1. then false else
    let coef = 1024. *. 1024. /. 4. in
    let max = float_of_int max_int in
    let k = 
      if max /. coef < limit 
      then max
      else (limit *. coef)
    in 
    let used = float_of_int ((Gc.quick_stat ()).Gc.heap_words) in
      if comp used k then true else false

let set_alarms log =

  (*Setting gc alarm for memory overflow*)
  let log =
    let str,w = 
      if !memory_limit > 0 then ("-Setting memory limit to "^(string_of_int !memory_limit)^"M",0) 
      else ("No memory limit defined",1) 
    in
      Session.add_log_entry w str log
  in  
    (*Setting gc alarm for strong collection mode*)
  let log =
    if !memory_limit > 0 then
      Session.add_log_entry 0 (sprintf "-Will trigger strong gc mode at %.1f of memory limit" !gc_high) log
    else log
  in
  let high =
    Gc.create_alarm 
      (fun () -> 
	 if test ((float_of_int !memory_limit) *. !gc_high) greater then
	   begin
	     gc_mode:=Some HIGH ;
	     Gc.set { (Gc.get()) with Gc.space_overhead = 0 (*default 80*) } ;
	     let overflow =
	       Gc.create_alarm 
		 (fun () -> 
		    if test (float_of_int !memory_limit) greater then
       		      begin
			Gc.compact() ;
			if test (float_of_int !memory_limit) greater then raise Error.Too_expensive
			else ()
		      end
		 )
	     in
	       gc_overflow := Some overflow ;
	   end
	 else
	   ()
      )
  in
    gc_alarm_high := Some high ;
    
    (*Setting gc alarm for weak collection mode*)
    let log =
      if !memory_limit > 0 then
	Session.add_log_entry 0 (sprintf "-Will set overhead to %d when at %.1f of memory limit" !gc_overhead !gc_low) log
      else log
    in
    let low =
      Gc.create_alarm 
	(fun () -> 
	   if test ((float_of_int !memory_limit) *. !gc_low) smaller then
	     begin
	       gc_mode:=Some LOW ;
	       Gc.set { (Gc.get()) with Gc.space_overhead = !gc_overhead} 
	     end
	)
    in
      Data.gc_alarm_low := Some low ;
      log

