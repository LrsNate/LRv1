let print_rule (name, words) = 
  Printf.printf "%s -> %s\n" name (String.concat ", " words)

let () =
  let rules = Files.read_rules "grammars/" in
  List.iter print_rule (List.map Grammar.rule_of_string rules)
