type state = int
type word = string
type transition = state * word * state

class automaton =
object (self)
  val _start: int = 1
  val mutable _next_state: int = 2
  val mutable _transitions: transition list = []
  val mutable _ends: (state * Grammar.rule) list = []

  method read_rule rule =
    let add_transition state sym =
      match self#find_transition state sym with
        | Some x -> x
        | None -> self#new_state state sym
    in
    let final_state =
      List.fold_left add_transition _start (snd rule)
    in
    self#close_rule final_state rule

  method recognize words =
    let words_iter state sym =
      Option.flatMap (fun s -> self#find_transition s sym) state
    in
    let final_state =
      List.fold_left words_iter (Some _start) words
    in
    Option.flatMap self#find_rule final_state

  method find_transition state sym =
    let rec find_aux state sym = function
      | [] -> None
      | (a, b, c) :: _ when a = state && b = sym -> Some c
      | _ :: tl -> find_aux state sym tl
    in
    find_aux state sym _transitions

  method new_state state sym =
    _transitions <- _transitions @ [state, sym, _next_state];
    _next_state <- _next_state + 1;
    _next_state - 1

  method close_rule state rule =
    _ends <- _ends @ [state, rule]

  method find_rule state =
    let rec find_aux state = function
      | [] -> None
      | (a, b) :: _ when a = state -> Some b
      | _ :: tl -> find_aux state tl
    in
    find_aux state _ends
end