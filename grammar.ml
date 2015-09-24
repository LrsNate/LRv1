type rule = string * string list
type grammar = rule list

let rule_of_string str =
  let words = Str.split (Str.regexp " ") str in 
  let rec build_rule name_acc = function
    | "::=" :: tl ->
      let name = String.concat " " (List.rev name_acc) in
      name, tl
    | hd :: tl -> build_rule (hd :: name_acc) tl
    | [] -> failwith "Parse error: missing rule separator."
  in build_rule [] words