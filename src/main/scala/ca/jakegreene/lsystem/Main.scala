package ca.jakegreene.lsystem

object Main extends App {
  val grammar = Grammar.deterministic(Set('A', 'B'), Map(('A' -> "AB"), ('B' -> "A")))
  println(grammar.produce("A", 4))
  
  val aToB = Rule.free('A' -> "B", 0.5)
  val aToC = Rule.free('A' -> "C", 0.5)
  val randomGrammar = Grammar.stochastic(Set('A', 'B', 'C'), Set(aToB, aToC))
  println(randomGrammar.produce("A" * 10, 1))
  
//  val aToBContext = ContextRule.left('A', "B", "D")
//  val aToCContext = ContextRule.right('A', "C", "D")
//  val contextualGrammar = Grammar.contextual(Set('A'), Set(aToBContext, aToCContext))
//  val result = contextualGrammar.produce("DAD", 1)
//  println(result)
  
  
  val syllableConstant = Rule.free('S' -> "c", 0.5)
  val syllableVowel = Rule.free('S' -> "v", 0.5)
  val syllableWait = Rule.left("S", 'S' -> "S")
  val syllableForceVowel = Rule.left("c", 'S' -> "v")
  val endExtends = Rule.left("c", 'E' -> "SE")
  val end = Rule.free('E' -> "S")
  val endWait = Rule.left("S", 'E' -> "E")
  val first = Rule.free('F' -> "SSS")
  val last = Rule.free('L' -> "SSSE")
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
    .rule('B' -> "CC").weight(0.5).right("A")
    .build()
  println(builtGrammar.produce("AABA"))
}