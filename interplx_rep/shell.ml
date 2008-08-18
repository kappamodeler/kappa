let version="1.0"
let usage_msg = "InterPlx "^version^": \n"^"Usage is interplx [option]"
let version_msg = "Interpretor by PLectiX: "^version^"\n"

let channel = ref stdin 

let main =
  let options = [ 
    ("--file", Arg.String (fun file -> channel:=open_in file), "use file instead of standard input channel");
  ]
  in
    Arg.parse options (fun _ -> Arg.usage options usage_msg ; exit 1) usage_msg ;
    
    let stream = Stream.of_channel !channel in
    let str = ref "" in
      while true do
	let c = Stream.next stream in 
	  if c = '\n' then 
	    begin
	      match !str with
		  "exit" -> (close_in !channel ; exit 0)
		| _ as rule_str -> 
		    try
		      let r_bf = Kappa_lex.make_rule rule_str in
			List.iter (fun r -> 
				     Rule.print_compil ~with_warning:true r
				  ) r_bf ; flush stdout ;
			str:=""
		    with
			Error.Syntax (msg,_) -> (Printf.printf "%s\n" msg ; flush stdout ; str:="")
	    end
	  else str:=Printf.sprintf "%s%c" !str c
      done 
