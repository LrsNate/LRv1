let print_rule (name, words) = 
  Printf.printf "%s -> %s\n" name (String.concat ", " words)

let () =
  let a = new Automaton.automaton in
  let rules = Files.read_rules "grammars/" in
  List.iter a#read_rule (List.map Grammar.rule_of_string rules);
  a#display
