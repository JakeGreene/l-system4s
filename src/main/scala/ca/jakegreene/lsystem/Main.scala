package ca.jakegreene.lsystem

object Main extends App {
  val grammar = Grammar.deterministic(Set('A', 'B'), Map(('A' -> "AB"), ('B' -> "A")))
  println(grammar.produce("A", 4))
  
  val aToB = Rule('A', "B", 0.5)
  val aToC = Rule('A', "C", 0.5)
  val randomGrammar = Grammar.stochastic(Set('A', 'B', 'C'), Set(aToB, aToC))
  println(randomGrammar.produce("A" * 10, 1))
  
  val aToBContext = ContextRule.left('A', "B", "D")
  val aToCContext = ContextRule.right('A', "C", "D")
  val contextualGrammar = Grammar.contextual(Set('A'), Set(aToBContext, aToCContext))
  val result = contextualGrammar.produce("DAD", 1)  
  println(result)
}