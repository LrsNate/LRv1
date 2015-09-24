type state = int
type word = string
type transition = state * word * state

class automaton =
object
  val start = 1
  val mutable transitions: transition list = []
  val mutable ends: (state * Grammar.rule) list = []
end