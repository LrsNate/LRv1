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

  method find_transition state sym =
    let rec find_aux state sym = function
      | [] -> None
      | (a, b, c) :: tl when a = state && b = sym -> Some c
      | _ :: tl -> find_aux state sym tl
    in
    find_aux state sym _transitions

  method new_state state sym =
    _transitions <- _transitions @ [state, sym, _next_state];
    _next_state <- _next_state + 1;
    _next_state - 1

  method close_rule state rule =
    _ends <- _ends @ [state, rule]

  method display =
    let print_trans (a, b, c) = Printf.printf "%d %s %d\n" a b c in
    List.iter print_trans _transitions


end