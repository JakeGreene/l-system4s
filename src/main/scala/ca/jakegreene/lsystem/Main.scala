package ca.jakegreene.lsystem

object Main extends App {
  val grammar = Grammar.deterministic(Set('A', 'B'), Map(('A' -> "AB"), ('B' -> "A")))
  println(grammar.produce("A", 4))
  
  val aToB = Rule('A', "B", 0.5)
  val aToC = Rule('A', "C", 0.5)
  val randomGrammar = Grammar.stochastic(Set('A', 'B', 'C'), Set(aToB, aToC))
  println(randomGrammar.produce("A" * 10, 1))
  
//  val aToBContext = ContextRule.left('A', "B", "D")
//  val aToCContext = ContextRule.right('A', "C", "D")
//  val contextualGrammar = Grammar.contextual(Set('A'), Set(aToBContext, aToCContext))
//  val result = contextualGrammar.produce("DAD", 1)
//  println(result)
  
  
  val syllableConstant = StochasticContextRule.free('S', "c", 0.5)
  val syllableVowel = StochasticContextRule.free('S', "v", 0.5)
  val syllableWait = StochasticContextRule.left('S', "S", 1.0, "S")
  val syllableForceVowel = StochasticContextRule.left('S', "v", 1.0, "c")
  val endExtends = StochasticContextRule.left('E', "SE", 1.0, "c")
  val end = StochasticContextRule.free('E', "S", 1.0)
  val endWait = StochasticContextRule.left('E', "E", 1.0, "S")
  val first = StochasticContextRule.free('F', "SSS", 1.0)
  val last = StochasticContextRule.free('L', "SSSE", 1.0)
  val stochasticContextualGrammar = Grammar.stochasticContextual(
    Set('S', 'F', 'L'),
    Set(syllableConstant,
        syllableVowel,
        syllableWait,
        syllableForceVowel,
        first,
        last,
        end,
        endExtends,
        endWait
     )
  )
  for (i <- 1 to 8) {
    println(stochasticContextualGrammar.produce("F L", i))    
  }
  // Fluent API
  // Auto-detect type based on passed rules
  //
  //val grammar = Grammar.variables('A', 'B')
  //  .rule('A' -> "B")
  //  .rule('A' -> "C")
  //  .rule('A' -> "BB").left("B")
  //  .rule('B' -> "CC").weight(0.5).right("A")
  //  .steps(5)
  //  .build()
  //val result = grammar.apply("AABA")
  
  val builtGrammar = new GrammarBuilder().variables('A', 'B')
    .rule('A' -> "B")
    .rule('A' -> "C")
    .rule('A' -> "BB").left("B")
    .rule('B' -> "CC").probability(0.5).right("A")
    .build()
  println(builtGrammar.produce("AABA"))
}