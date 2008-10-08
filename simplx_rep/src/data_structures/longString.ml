type t = char list

let empty:t = []

let is_empty ls = match ls with [] -> true | _ -> false

let concat ?sep str ls =
  let pls = ref ls in
  let _ = 
    match sep with
	None -> ()
      | Some c -> if not (is_empty ls) then pls:=(c::!pls) 
  in
    begin
      String.iter (fun c -> pls:=(c::!pls)) str ;
      (!pls)
    end

let print ls = List.fold_left (fun _ c -> print_char c) () (List.rev ls)

let printf d ls = 
  let ls = List.rev ls in
    List.fold_left (fun _ c -> Printf.fprintf d "%c" c) () ls 
  
let to_string ls = (*costy but just to check!*)
  let ls = List.rev ls in
    List.fold_left (fun str c -> Printf.sprintf "%s%c" str c) "" ls
