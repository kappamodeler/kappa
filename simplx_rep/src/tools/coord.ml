type t = int 

let max_x = max_int lsr 2
let max_y = 4 (*2 bits*)

let pi0 coord = coord lsr 2 
let pi1 coord = coord land 3

let to_pair coord = (pi0 coord,pi1 coord)
let of_pair (x,y) = 
  if (x > max_x) or (y>max_y) then (failwith "Coordinate overflow")
  else 
    (x lsl 2) lor y
