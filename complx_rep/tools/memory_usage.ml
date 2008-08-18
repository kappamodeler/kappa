(* 28/06/2007 *)
(* Static analysis of BNG systems*)
(* Jerome Feret pour PlectiX *)
(* Safe Memory limitation *)
(* memory_usage.ml *)


let memory_alarm = ref None
let cancel_alarm () = 
  match
    (!memory_alarm) 
  with
    None -> ()
  | Some a -> Gc.delete_alarm a

let set_memory_usage i memory_overflow  = 
  let _ = cancel_alarm () in
  if i<1 
    then 
    memory_alarm := None
  else
    (let coef = 1024 * 1024 / 4 in
     let k = 
         if max_int / coef < i 
	 then max_int else 
	   i * coef in 
     memory_alarm := Some (Gc.create_alarm 
			     (fun () -> 
			       if (Gc.quick_stat ()).Gc.heap_words > k
			       then memory_overflow  ())))
      
