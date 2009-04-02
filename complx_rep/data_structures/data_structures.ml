(* 2/10/2006 *)
(* Static analysis of BNG systems*)
(* Jerome Feret pour PlectiX *)
(* Data structures *)
(* data_structures.ml *)
open Pb_sig
open Map2
  
type s4 = (*PL of*) step list  (*| PR of string * int*)


module StringSet=StringSet
module IntMap = IntMap
module IntSet = IntSet
module Int22Map = Map2.Make (struct type t = (int*int)*(int*int) let compare = compare end)
module Int3Map = Map2.Make (struct type t = int*int*int let compare = compare end)
module Int3Set = Set.Make (struct type t = int*int*int let compare = compare end)
module Int33Map = Map2.Make (struct type t = (int*int*int)*(int*int*int) let compare = compare end)
module Intmap = Map2.Make (struct type t = int let compare=compare end)

(*module String2Map = Map2.Make (struct type t = string*string let compare = compare end)*)
module String2Set = Set.Make (struct type t = string*string let compare = compare end)
module IntStringSet = Set.Make (struct type t = int*string let compare=compare end)
module IntStringMap = Map2.Make (struct type t = int*string let compare = compare end)
module S4 = Map.Make (struct type t = s4 let compare=compare  end)
module StringMap = StringMap
module String4Set = Set.Make (struct type t = (string*string)*(string*string) let compare = compare end)
(*module String2Set = Set.Make (struct type t = string*string let compare = compare end)*)
module String4Map = Map2.Make (struct  type t = (string*string)*(string*string) let compare = compare end)
module String3Set = Set.Make (struct type t = string*int*string let compare = compare end)
module STRING3Set = Set.Make (struct type t = string*string*string let compare = compare end)
(*module String2Map = Map2.Make (struct type t = string*string let compare = compare end)*)
module StringListMap = Map2.Make(struct type t = string list let compare = compare end)
module String2ListSet = Set.Make (struct type t = (string*string) list let compare= compare end)
module StringListSet = Set.Make (struct type t = string list let compare= compare end)
(*module String22Set = Set.Make (struct type t = (string*string)*(string*string) let compare = compare end)*)
module IntListMap = Map2.Make (struct type t = int list let compare = compare end)
module IntListSet = Set.Make (struct type t = int list let compare = compare end)
module StringBListMap = Map2.Make (struct type t = string * ((b*bool) list) let compare = compare end)


type compile = Smashed | Unsmashed 

module Int2Set = Set.Make (struct type t = int*int let compare = compare end)

type kleenean_token = TRUE | FALSE | ANY


let h_known x = 
  match x with Not_initialized | Abstracted -> false
| _ -> true

let h_data x = 
  match x with Init _ -> true
  | _ -> false 

let h_init x = 
  match x with Not_initialized -> false
  | _ -> true 
let h_all = fun _ -> true

let h hflag = 
  match hflag with 
    KNOWN -> h_known
  | DATA -> h_data
  | INIT -> h_init
  | ALL -> h_all 

module SiteMap = Map2.Make (struct type t = site let compare = compare end)
module SiteSet = Set.Make (struct type t = site let compare = compare end)





let interesting p pf =
    (h pf.f_is_marked p.is_marked) or (h pf.f_mark p.mark) or (h pf.f_impossible_marks p.impossible_marks) or (h pf.f_is_bound p.is_bound) or (h pf.f_link p.link) or (h pf.f_impossible_links p.impossible_links)

let tuple_init  = 
  {f_is_marked=INIT;
   f_mark=INIT;
   f_impossible_marks=INIT;
   f_is_bound=INIT;
   f_link=INIT;
   f_impossible_links=INIT}

let tuple_known = 
 {f_is_marked=KNOWN;
  f_mark=KNOWN;
  f_impossible_marks=KNOWN;
  f_is_bound=KNOWN;
  f_link=KNOWN;
  f_impossible_links=KNOWN}
 
let tuple_all =
 {f_is_marked=ALL;
  f_mark=ALL;
  f_impossible_marks=ALL;
  f_is_bound=ALL;
  f_link=ALL;
  f_impossible_links=ALL}

let tuple_bot =
 {is_marked=Not_initialized;
  mark=Not_initialized;
  impossible_marks=Not_initialized;
  is_bound=Not_initialized;
  link=Not_initialized;
  impossible_links=Not_initialized}
  
    
 
let tuple_data =
 {f_is_marked=DATA;
  f_mark=DATA;
  f_impossible_marks=DATA;
  f_is_bound=DATA;
  f_link=DATA;
  f_impossible_links=DATA}

module Arraymap = Unbounded_array.ArrayExt

let parser_line = ref 0 
