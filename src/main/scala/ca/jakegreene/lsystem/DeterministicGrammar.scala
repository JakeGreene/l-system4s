package ca.jakegreene.lsystem

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