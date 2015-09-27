let rec input_loop au =
  try
    begin
      let s = input_line stdin in
      let words = Str.split (Str.regexp " ") s in
      match au#recognize words with
        | Some rule -> print_endline (fst rule); input_loop au
        | None -> print_endline "I'm sorry, what was that?"; input_loop au
    end
  with
    | End_of_file -> print_endline "Bye!"

let () =
  let debug =
    let argv = Array.to_list Sys.argv in
    List.exists ((=) "--debug") argv
  in
  let a = new Automaton.automaton debug in
  let rules = Files.read_rules "grammars/" in
  List.iter a#read_rule (List.map Grammar.rule_of_string rules);
  input_loop a
