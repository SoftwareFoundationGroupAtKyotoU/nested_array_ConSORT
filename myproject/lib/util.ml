exception Error of string
let err s = raise (Error s)

let rec lookup x env =
  try List.assoc x env with Not_found -> err ("variable not bound: " ^ x)