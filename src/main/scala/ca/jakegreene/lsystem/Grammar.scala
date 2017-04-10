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

case class DeterministicGrammar(variables: Set[Char], rules: Map[Char, String]) extends Grammar {

  def produce(axiom: String, steps: Int = 100): String = {
    var result = axiom
    for (i <- 1 to steps) {
      result = result.flatMap {
        case v if rules.isDefinedAt(v) => rules(v)
        case cons => cons.toString
      }
    }
    result
  }
}