let map f = function
  | Some x -> Some (f x)
  | None -> None

let flat_map f = function
  | Some x -> f x
  | None -> None

let get = function
  | Some e -> e
  | None -> failwith "None.get"

let get_or_else x = function
  | Some e -> e
  | None -> x

let is_defined = function
  | Some _ -> true
  | None -> false