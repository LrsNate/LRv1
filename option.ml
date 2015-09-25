let map f = function
  | Some x -> Some (f x)
  | None -> None

let flatMap f = function
  | Some x -> f x
  | None -> None

let getOrElse x = function
  | Some e -> e
  | None -> x