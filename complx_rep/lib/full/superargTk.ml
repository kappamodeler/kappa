(* LablTK GUI for option selection Superarg.

   Copyright (C) Antoine Mine' 2006
 *)

open Data_structures
open Superarg
open Tk

let map = ref StringMap.empty   (* key => entry widget *)
let fmap = ref StringMap.empty  (* key => frame widget *)

(* show / hide options according to current mode *)
let set_visibility (a:t) =
  List.iter 
    (fun (key,_,_,_,lvl) ->
      print_string key;
      try
	let f = StringMap.find key !fmap in
	if show_level lvl
	then 
          (print_string "YES\n";List.iter (fun f -> pack ~side:`Top ~anchor:`W [coe f]) f)
	else (print_string "NO\n";List.iter (fun f -> Pack.forget [coe f]) f)
      with Not_found -> (print_string "Not_found\n")
    ) a


(* option value => widget value *)
let widget_update_from_spec (a:t) =
  let set n v =
    try Textvariable.set (StringMap.find n !map) v
    with Not_found -> ()
  in
  List.iter 
    (fun (key,spec,msg,cat,lvl) ->
      match spec with
      |	Void -> ()
      | Bool r -> set key (if !r then "1" else "0")
      | Int r -> set key (string_of_int (!r))
      | Int_opt r -> 
	  set key (match !r with None -> "" | Some i -> string_of_int i)
      | String r -> set key !r
      | String_opt r -> set key (match !r with None -> "" | Some s -> s)
      | String_list r -> set key (cat_list !r " ")
      | Float r -> set key (string_of_float (!r))
      | Float_opt r -> 
	  set key (match !r with None -> "" | Some f -> string_of_float f)
      | Choice (_,r) -> set key !r
      | Choice_list (l,r) -> 
	  List.iter 
	    (fun (k,_) -> set (key^"."^k) (if List.mem k !r then "1" else "0"))
	    l
      | Multi (x,[]) -> ()
      | Multi (x,y) -> set key ""
      |	MultiExt _ -> set key ""
    ) a

(* command-line argument => widget value *)
let widget_update_from_cmd (a:t) (l:string list) =
  let rec doit accum = function
      [] -> accum
    | ("-help" | "--help" | "-h" | "--gui")::rem -> doit accum rem
    | "--expert"::rem -> 
	expert_mode := true; set_visibility a; doit accum rem
    | "--no-expert"::rem -> 
	expert_mode := false; set_visibility a; doit accum rem
    | opt::rem ->
	if opt="-" then List.rev_append rem accum else
	try 
	  let key,spec,_,_,_ =
	    List.find (fun (key,_,_,_,_) -> opt=key || opt=(nokey key)) a in
	  let set n v = Textvariable.set (StringMap.find n !map) v
	  and get n = Textvariable.get (StringMap.find n !map) in
	  let rem = match spec,rem with
	  | Void , rem -> rem
	  | Bool _, rem -> set key (if opt=key then "1" else "0"); rem
	  | (Int _ | Int_opt _ | String _ | String_opt _ | Float _ |
	    Float_opt _ | Choice _ ), (v::rem) when opt=key ->
	      set key v; rem
	  | String_list _,  (v::rem) when opt=key ->
	      set key ((get key)^" "^v); rem
	  | (Int _ | Int_opt _ | String _ | String_opt _ | Float _ |
	    Float_opt _ | Choice _ | String_list _), rem ->
	      set key ""; rem
	  | Choice_list _, (v::rem) when opt=key ->
	      set (key^"."^v) "1"; rem
	  | Choice_list (l,_), rem ->
	      List.iter (fun (v,_) -> set (key^"."^v) "0") l; rem
	  | Multi (x,[]), rem -> ignore (doit [] x); rem
	  | Multi (x,y), (v::rem) ->
	      set key v;
	      if v<>"" then
		(ignore (doit [] x);
		 ignore (List.fold_left (fun accum l -> doit [] [l;v])
			   accum y));
	      rem
	  | MultiExt l,(v::rem) -> 
	      set key v;
	      if v<>"" then 
		(ignore (List.fold_left (fun accum (l,ext) -> doit [] [l;v^ext])
			  accum l));
	      rem
	  | _, [] -> rem
	  in
	  doit accum rem (* option eaten *)
	with _ -> doit (opt::accum) rem (* option accumulated *)
  in List.rev (doit [] l)
    


(* widget value => command-line argument,
   if [short]=[true] no command is output is the value is the default one *)
let cmd_of_widget (a:t) (short:bool) =
  let get n = Textvariable.get (StringMap.find n !map) in
  List.fold_left
    (fun accum (key,spec,msg,cat,lvl) ->
      try match spec with
      |	Void -> accum
      | Bool r ->
	  let v = (get key = "1") in
	  if !r=v && short then accum else
	  if v then key::accum else (nokey key)::accum
      | Int r ->
	  let v = get key in
	  if !r=(int_of_string v) && short then accum else key::v::accum
      | Int_opt r ->
	  let v = get key in
	  if v="" && (!r<>None || not short) then (nokey key)::accum 
	  else if v<>"" && (!r<>(Some (int_of_string v)) || not short) 
	  then key::v::accum else accum
      | String r ->
	  let v = get key in
	  if v="" && (!r<>"" || not short) then (nokey key)::accum
	  else if v<>"" && (!r<>v || not short) 
	  then key::v::accum else accum
      | String_opt r ->
	  let v = get key in
	  if v="" && (!r<>None || not short) then (nokey key)::accum
	  else if v<>"" && (!r<>Some v || not short) 
	  then key::v::accum else accum
      | String_list r ->
	  let v = cut_list (get key) in
	  if v = !r && short then accum 
	  else if v = [] then (nokey key)::accum
	  else List.fold_right (fun x accum -> key::x::accum) v accum
      | Float r ->
	  let v = get key in
	  if !r=(float_of_string v) && short then accum else key::v::accum
      | Float_opt r ->
	  let v = get key in
	  if v="" && (!r<>None || not short) then (nokey key)::accum 
	  else if v<>"" && (!r<>(Some (float_of_string v)) || not short) 
	  then key::v::accum else accum
      | Choice (ll,r) ->
	  let v = get key in
	  if not (List.exists (fun (k,_) -> v=k) ll) 
	  then failwith "invalid choice"
	  else if !r=v && short then accum else key::v::accum
      | Choice_list (ll,r) ->
	  let v = 
	    List.fold_left 
	      (fun accum (k,_) -> 
		try if get (key^"."^k) = "1" then k::accum else accum
		with Not_found -> accum
	      ) [] ll
	  in
	  if !r=v && short then accum
	  else if v=[] then (nokey key)::accum
	  else List.fold_left (fun accum x -> key::x::accum) accum v
      | Multi _ -> accum
      |	MultiExt _ -> accum
      with 
	
      | Not_found -> accum
      | Failure f -> failwith ("Invalid argument type for option "^key^" in "^(
			       List.fold_left (fun sol x -> sol^" "^x) "" cat)^": "^f)
    ) [] (List.rev a)
    


let balloon_delay = 100

(* create an option widget, add the defined variables to the map *)
let widget_of_spec (a:t) key spec msg lvl parent = 
  let f = Frame.create parent in
  let _ = fmap := 
    begin 
      let old = 
        try
          StringMap.find key !fmap
        with 
          _ -> []
      in  StringMap.add key (f::old) !fmap 
    end
  in 
  let v = 
    try (StringMap.find key (!map))
    with Not_found 
      -> (Textvariable.create ())
  in 
  (match spec with
  | Bool _ ->
      let chk = Checkbutton.create ~variable:v ~text:key f in
      pack ~side:`Left ~anchor:`W [chk];
      Balloon.put ~on:(coe chk) ~ms:balloon_delay msg;
      map := StringMap.add key v !map
  | Void -> 
      let lbl = Label.create ~text:(" ") ~padx:20 f in 
      pack ~side:`Left ~expand:true ~fill:`X ~anchor:`W [coe lbl];
      Balloon.put ~on:(coe lbl) ~ms:balloon_delay msg;
  | Int _ | Int_opt _ | String _ | String_opt _ | String_list _ 
  | Float _ | Float_opt _ ->
      let ext = match spec with
      | Int _ | Int_opt _ -> "<int>"
      | String _ | String_opt _ -> "<name>"
      | String_list _ -> "<names> ..."
      | Float _ | Float_opt _ -> "<float>"
      | _ -> ""
      in
      let lbl = Label.create ~text:(key^" "^ext) ~padx:20 f
      and entry = Entry.create ~textvariable:v f in
      pack ~side:`Left ~expand:true ~fill:`X ~anchor:`W [coe lbl;coe entry];
      Balloon.put ~on:(coe lbl) ~ms:balloon_delay msg;
      Balloon.put ~on:(coe entry) ~ms:balloon_delay msg;
      map := StringMap.add key v !map
  | Choice (l,_) ->
      let lbl = Label.create ~text:key ~padx:20 f in
      let fff = Frame.create f in
      let ff = Frame.create fff in
      let p = ref 0 in
      List.iter
	(fun (k,msg2) ->
	  let radio = Radiobutton.create 
	      ~variable:v ~text:k ~value:k ~padx:40 ff in
	  Grid.configure ~sticky:"w" ~column:(!p mod 3) ~row:(!p / 3) [radio];
	  Balloon.put ~on:(coe radio) ~ms:balloon_delay (msg^":\n"^msg2);
 	  incr p
	) l;
      pack ~side:`Top ~anchor:`W [coe lbl; coe ff];
      pack ~side:`Left ~anchor:`W [coe fff];
      Balloon.put ~on:(coe lbl) ~ms:balloon_delay msg;
      map := StringMap.add key v !map
  | Choice_list (l,_) ->
      let lbl = Label.create ~text:key ~padx:20 f in
      let fff = Frame.create f in
      let ff = Frame.create fff in
      let p = ref 0 in
      let nb = 4 in (* number of columns *)
      List.iter
	(fun (k,msg2) ->
	  let v = Textvariable.create () in
	  let chk = 
	    Checkbutton.create ~variable:v ~text:k ~padx:40 ff in
	  Grid.configure ~sticky:"w" ~column:(!p mod nb) ~row:(!p / nb) [chk];
	  incr p;
	  Balloon.put ~on:(coe chk) ~ms:balloon_delay (msg^":\n"^msg2);
	  map := StringMap.add (key^"."^k) v !map
	) l;
      pack ~side:`Top ~anchor:`W [coe lbl; coe ff];
      pack ~side:`Left ~anchor:`W [coe fff];
      Balloon.put ~on:(coe lbl) ~ms:balloon_delay msg
  | Multi (x,[]) ->
      let update () = ignore (widget_update_from_cmd a x) in
      let b = Button.create ~text:key ~padx:20 ~command:update f in
      pack ~side:`Left ~anchor:`W [coe b];
      let msg2 = msg^"\n(equivalent to "^(cat_list x " ")^" )" in
      Balloon.put ~on:(coe b) ~ms:balloon_delay msg2;
      map := StringMap.add key v !map
   | Multi (x,y) ->
      let rec update () = 
	let s = Textvariable.get v in
	if s<>"" then
	  (ignore (widget_update_from_cmd a x);
	   List.iter (fun o -> ignore (widget_update_from_cmd a [o;s])) y);
	Textvariable.handle v ~callback:update
      in
      let lbl = Label.create ~text:key ~padx:20 f
      and entry = Entry.create ~textvariable:v f in
      pack ~side:`Left ~expand:true ~fill:`X ~anchor:`W [coe lbl;coe entry];
      let msg2 = msg^"\n(equivalent to "^(cat_list (x@y) " ")^" )" in
      Balloon.put ~on:(coe lbl) ~ms:balloon_delay msg2;
      Balloon.put ~on:(coe entry) ~ms:balloon_delay msg2;
      Textvariable.handle v ~callback:update;
      map := StringMap.add key v !map
   | MultiExt l -> 
       let rec update () = 
	let s = Textvariable.get v in
	if s<>"" then
	  (
	  List.iter (fun (o,ext) -> ignore (widget_update_from_cmd a [o;s^ext])) l);
	Textvariable.handle v ~callback:update
      in
       let lbl = Label.create ~text:key ~padx:20 f
       and entry = Entry.create ~textvariable:v f in
       pack ~side:`Left ~expand:true ~fill:`X ~anchor:`W [coe lbl;coe entry];
       let msg2 = msg^"\n(equivalent to "^(cat_list (List.rev_map snd (List.rev l)) " ")^" )" in
       Balloon.put ~on:(coe lbl) ~ms:balloon_delay msg2;
       Balloon.put ~on:(coe entry) ~ms:balloon_delay msg2;
       Textvariable.handle v ~callback:update;
       map := StringMap.add key v !map
   );

  let text = match lvl with 
  | Expert -> "(expert)"
  | Developper -> "(developper)"
  | _ -> ""
  in 
  if text<>"" then (
    let l = Label.create ~text f in
    Balloon.put ~on:(coe l) ~ms:balloon_delay msg;
    pack ~side:`Left ~anchor:`W [l]
   );
  pack ~side:`Top ~anchor:`W [coe f]



(* notebook *)
class pager bparent fparent =
  let bars = Frame.create bparent in  (* multi-line button bar *)
  let bar = ref (Frame.create bars)  (* one button line *)
  and barsize = ref 0
  and maxbarwidth = 65               (* split bar at this column (in chars) *)
  and cont = Frame.create fparent    (* current page *)
  and cur = ref ""                   (* current page name *)
  and pages = ref StringMap.empty in (* all pages *)

  object (self)
    
    initializer 
      pack ~side:`Top [coe !bar];
      pack ~side:`Top [coe bars];
      pack ~side:`Top ~pady:5 ~fill:`Both ~expand:true [coe cont]

    (* sets the page currently viewed *)
    method set_page name = 
      (try 
	let fr,_,b = StringMap.find !cur !pages in
	cur := "";
	Button.configure ~relief:`Raised b;
	Pack.forget [coe fr];
      with Not_found -> ());
      (try 
	let fr,_,b = StringMap.find name !pages in
	cur := name;
	Button.configure ~relief:`Sunken b;
	pack ~expand:true ~fill:`Both ~anchor:`Center [coe fr];
      with Not_found -> ())


    (* gets a page (create if non existing) *)
    method get_page name : Widget.frame Widget.widget =
      try let _,p,_ = StringMap.find name !pages in p
      with Not_found ->
	if !barsize/maxbarwidth <> (!barsize+String.length name)/maxbarwidth
	then (bar := Frame.create bars; pack ~side:`Top [coe !bar]);
	let fr = Frame.create ~borderwidth:2 ~relief:`Ridge cont 
	and lbl = Button.create
	    ~text:name ~command:(fun () -> self#set_page name) !bar in
	let p = Frame.create fr in
	pack ~side:`Left ~padx:1 ~pady:1 [coe lbl];
	Grid.column_configure ~minsize:600 (coe fr) 0;
	Grid.row_configure ~minsize:400 (coe fr) 0;
	Grid.configure ~column:0 ~row:0 [coe p];
	barsize := !barsize + String.length name;
	pages := StringMap.add name (fr,p,lbl) !pages;
	if !cur = "" then self#set_page name;
	p

  end


let build_spec (a:t) bparent fparent =
  let opts = new pager bparent fparent in
  StringMap.iter 
    (fun _ l ->
      List.iter
	(fun (key,spec,msg,cat,lvl) -> 
	  List.iter (fun cat -> 
	    widget_of_spec a key spec msg lvl (opts#get_page cat)
	      ) cat) l ) (order a);
  widget_update_from_spec a


exception Exit of string list

(* main *)
let gui (a:t) (args:string list) : string list =

  let top = openTk () in
  appname_set "The PLECTIX Static Analyzer";
  Balloon.init ();
  (* option list *)
  let up = Frame.create top in
  let left = Frame.create up
  and right = Frame.create up
  and middle = Frame.create top in
  pack ~side:`Top [up];
  pack ~side:`Top ~expand:true ~fill:`Both [middle];  
  pack ~side:`Left ~padx:20 [left; right];
  build_spec a right middle;

  (* expert mode *)
  let expyes = Radiobutton.create ~text:"Expert" ~value:"1"
      ~command:(fun () -> expert_mode := true; set_visibility a) left
  and expno = Radiobutton.create ~text:"Normal" ~value:"0"
      ~command:(fun () -> expert_mode := false; set_visibility a) left in
  pack ~side:`Top ~anchor:`W [expno;expyes];

  (* file list *)
  let v = Textvariable.create () in
  let eframe = Frame.create top in
  let lbl = Label.create ~text:"Filenames: " eframe
  and entry = Entry.create ~width:80 ~textvariable:v eframe

  and but1 = Button.create ~text:"Add"
      ~command:(fun _ ->
	Fileselect.f 
	  ~title:"Add filenames"
	  ~action:(fun l ->
	    Textvariable.set v ((Textvariable.get v)^" "^(cat_list l " ")))
	  ~filter:"*.ka" ~file:"" ~multi:true ~sync:true
	       ) eframe

  and but2 = Button.create ~text:"Clear" 
      ~command:(fun _ -> Textvariable.set v "") eframe

  in
  pack ~side:`Left ~expand:true ~fill:`X 
    [coe lbl; coe entry; coe but1; coe but2];
  pack ~side:`Top ~pady:10 ~expand:true ~fill:`Both [coe eframe];
  
  (* extract command-line from widget values *)
  let cmd () = (cmd_of_widget a true)@(cut_list (Textvariable.get v)) in

  (* backup save that ignores errors *)
  let backup name =
    try 
      let f = open_out name in
      output_string f (cat_list (cmd ()) " ");
      close_out f
    with _ -> ()
  in

  (* buttons *)
  let bframe = Frame.create top in (* button bar *)
  let do_launch = ref false in

  let quit = Button.create ~text: "Quit" ~command:(fun _ -> closeTk ()) bframe 

  and reset = Button.create 
      ~text: "Reset to default" 
      ~command:(fun _ ->  
	backup "autosave_pre_reset.options"; 
	widget_update_from_spec a) bframe

  and import = Button.create 
      ~text: "Import options" 
      ~command:(fun _ -> 
	Fileselect.f 
	  ~title:"Merge options from file"
	  ~action:(function
	      [name] -> 
		(try
		  let f = open_in name in
		  let b = Buffer.create 128 in
		  (try while true do
		    Buffer.add_string b (input_line f);
		    Buffer.add_char b ' '
		  done with End_of_file -> ());
		  close_in f;
		  let x = cut_list (Buffer.contents b) in
		  backup "autosave_pre_import.options";
		  let rem = widget_update_from_cmd a x in
		  Textvariable.set v ((Textvariable.get v)^" "^
				      (cat_list rem " "))
		with exc ->
		  ignore 
		    (Dialog.create ~parent:(coe top) ~title:"Cannot load!" 
		       ~message:(Printexc.to_string exc) ~buttons:["Close"] 
		       ()) )
	    | _ -> ()
		  )
	  ~filter:"*.options" ~file:"default.options" ~multi:false ~sync:true )
      bframe

  and save = Button.create 
      ~text: "Save options"
      ~command:(fun _ -> 
	try
	  let result = cat_list (cmd ())" " in
	  Fileselect.f 
	    ~title:"Save file"
	    ~action:(function
		[name] -> 
		  (try
		    let f = open_out name in
		    output_string f result;
		    close_out f
		  with exc ->
		    ignore 
		      (Dialog.create ~parent:(coe top) ~title:"Cannot save!" 
			 ~message:(Printexc.to_string exc) ~buttons:["Close"] 
			 ()) )
	      | _ -> () )
	    ~filter:"*.options" ~file:"default.options" ~multi:false ~sync:true
	with exc ->
	  ignore 
	    (Dialog.create ~parent:(coe top) ~title:"Cannot save!" 
	       ~message:(Printexc.to_string exc) ~buttons:["Close"] 
	       ()) )
      bframe

  and go   = Button.create 
      ~text: "Launch analyze"
      ~command:(fun _ -> 
	try let _ = cmd () in do_launch := true; closeTk() 
	with exc ->
	  ignore 
	    (Dialog.create ~parent:(coe top) ~title:"Cannot launch analysis!" 
	       ~message:(Printexc.to_string exc) ~buttons:["Close"] 
	       ())
	       ) bframe
  in

  pack ~side:`Left ~padx:40 ~fill:`X ~expand:true [quit;reset;import;save;go];
  pack ~side:`Top ~fill:`Both ~expand:true [bframe];

  (* get command-line arguments *)
  let rem = widget_update_from_cmd a args in
  Textvariable.set v ((Textvariable.get v)^" "^(cat_list rem " "));
  Radiobutton.select (if !expert_mode then expyes else expno);
  set_visibility a;

  (* tk loop *)
  mainLoop ();

  (* back from gui *)
  if not !do_launch then (backup "autosave_pre_quit.options"; exit 0);
  backup "autosave_pre_launch.options"; 
  Printf.printf "/* The GUI launches the analysis with the options:\n%s\n*/\n" (cat_list (cmd ()) " "); flush stdout;
  Superarg.parse_list a (cmd ())


(* MAIN *)
(* **** *)

let parse (a:t) (def:string list ref) =
  check a;
  (* drop the first command-line argument: it is the executable name *)
  let args = List.tl (Array.to_list Sys.argv) in
  (* if no argument or "--gui" given, launch the gui, otherwise, parse args *)
  let rem = 
    if args=[] || List.exists ((=) "--gui") args 
    then gui a args else parse_list a args
  in
  if rem<>[] then def := rem
