package ca.jakegreene.lsystem

case class DeterministicGrammar(variables: Set[Char], rules: Set[Rule]) extends Grammar {

  val rulesByVar = rules.groupBy(_.input)
  
  def produce(axiom: String, steps: Int = 100): String = {
    var result = axiom
    for (i <- 1 to steps) {
      result = result.flatMap {
        case v if rulesByVar.isDefinedAt(v) =>
          val validRules = rulesByVar(v)
          if (validRules.isEmpty) {
            v.toString
          } else if (validRules.size == 1) {
            val rule = validRules.head
            rule.output
          } else {
            throw new IllegalArgumentException(
              s"""Ambiguous rules. Rules $validRules all match for variable $v in context ${result} at step $i"""
            )
          }
        case cons => cons.toString
      }
    }
    result
  }
}