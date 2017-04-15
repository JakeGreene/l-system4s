package ca.jakegreene.lsystem

object Grammar {
  def deterministic(variables: Set[Char], rules: Map[Char, String]): Grammar = {
    new DeterministicGrammar(variables, rules)
  }
  
  def stochastic(variables: Set[Char], rules: Set[Rule]): Grammar = {
    new StochasticGrammar(variables, rules)
  }
  
  def contextual(variables: Set[Char], rules: Set[ContextRule]): Grammar = {
    new ContextSensitiveGrammar(variables, rules)
  }
  
  def stochasticContextual(variables: Set[Char], rules: Set[StochasticContextRule]): Grammar = {
    new StochasticContextSensitiveGrammar(variables, rules)
  }
}

trait Grammar {
  def produce(axiom: String, steps: Int = 100): String
}

trait Stochastic
trait Deterministic

trait ContextFree
trait ContextSensitive