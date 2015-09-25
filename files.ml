let get_grammar_files path =
  List.map ((^) path) (Array.to_list (Sys.readdir path))

let read_rules folder_path =
  let files = get_grammar_files folder_path in
  let read_file file_path =
    let ic = open_in file_path in
    let rec build_lines chan acc =
      try
        build_lines chan (input_line chan :: acc)
      with
        | End_of_file -> acc
      in 
    let res = build_lines ic [] in 
    close_in ic;
    res
  in
  List.flatten (List.map read_file files)

