type t = int 

let bit_number_for_cc = 10

let max_x = max_int lsr bit_number_for_cc
let max_y = int_of_float (2. ** (float_of_int bit_number_for_cc))-1

let pi0 coord = coord lsr bit_number_for_cc 
let pi1 coord = coord land max_y

let to_pair coord = (pi0 coord,pi1 coord)
let of_pair (x,y) = 
  if (x > max_x) or (y>max_y) then (failwith "Coordinate overflow")
  else 
    (x lsl bit_number_for_cc) lor y

let to_string coord =
  let x,y = to_pair coord in
    Printf.sprintf "(%d,%d)" x y
