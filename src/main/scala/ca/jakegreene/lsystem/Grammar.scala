package ca.jakegreene.lsystem

object Grammar {
  def deterministic(variables: Set[Char], rules: Map[Char, String]): Grammar = {
    new DeterministicGrammar(variables, rules)
  }
}

trait Grammar {
  val variables: Set[Char]
  val rules: Map[Char, String]
  def produce(axiom: String, steps: Int = 100): String
}