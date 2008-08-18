let bench_mode = ref false

type param = I of int | F of float
exception True
exception False

module StringSet = Set.Make(struct type t = string let compare = compare end) (*interface={s1:string,...,sn:string}*)
module StringMap = Map_ext.Make(struct type t = string let compare = compare end) (*etat(s):int*)

module IntMap = Map_ext.Make(struct type t = int let compare = compare end)
module IntSet = Set.Make(struct type t = int let compare = compare end)

module FloatMap = Map_ext.Make(struct type t = float let compare = compare end)

module PortMap = Map_ext.Make(struct type t = (int*string) let compare = compare end)
module PortSet = Set.Make(struct type t = (int*string) let compare = compare end)

module CoordMap = Map_ext.Make(struct type t = int list let compare = compare end)
module CoordSet = Set.Make(struct type t = int list let compare = compare end)

type assoc = int IntMap.t

module AssocArray = Array_ext.Make(struct type t = assoc let default = IntMap.empty end)
module CoordSetArray = Array_ext.Make(struct type t = CoordSet.t let default = CoordSet.empty end)


module Palette:
sig
  type t  
  type color = (float*float*float)
  val find : int -> t -> color 
  val add : int -> color -> t -> t
  val mem : int -> t -> bool
  val empty : t
  val new_color : unit -> color
  val grey : int -> string
  val string_of_color : color -> string
end = 
struct 
  type color = (float*float*float)
  type t = color IntMap.t 
  let find = IntMap.find 
  let add = IntMap.add
  let mem = IntMap.mem
  let empty = IntMap.empty
  let new_color () = Random.float 1.0,Random.float 1.0,Random.float 1.0
  let grey d = if d > 16 then "black" else ("gray"^(string_of_int (100-6*d))) 
  let string_of_color (r,g,b) =  String.concat "," (List.map string_of_float [r;g;b])
end

(*type 'a option = None | Some of 'a*)

let color_of_mrk = function (*define here which color should be associated with internal state markers*)
    "p" -> "goldenrod1" (*phosphorylation*)
  | "u" -> "darkturquoise" (*unmarked*)
  | _ -> "gray" (*default values*)


let string_of_set f fold set = 
  let l = 
    fold (fun i cont -> (f i)::cont) set [] 
  in
    Printf.sprintf "{%s}" (String.concat "," l)


let string_of_map f1 f2 fold map = 
  let l = 
    fold (fun i j cont -> ((f1 i)^"->"^(f2 j))::cont) map [] 
  in
    Printf.sprintf "[%s]" (String.concat "," l)

let string_of_port (i,s) = Printf.sprintf "(%d,%s)" i s

let string_of_coord l = 
  let l = List.map string_of_int l in Printf.sprintf "(%s)" (String.concat "," l)

let rec sorted_insert a l =
  match l with
      h::tl -> if (compare a h) > 0 then h::(sorted_insert a tl) else a::l
    | [] -> [a]

let chop_extension f = try Filename.chop_extension f with Invalid_argument _ -> f

let gettime() = Sys.time()
 
let chrono t0 = Sys.time() -. t0
		
(*returns dt according to given activity and number of clashes*)      
let random_time_advance a clashes = 
  let rec lambda clashes acc = 
    if clashes < 0 then acc
    else
      let u = Random.float 1.0 in
	lambda (clashes-1) (acc -. (log u/.a))
  in
    lambda clashes 0.0
	  
    
