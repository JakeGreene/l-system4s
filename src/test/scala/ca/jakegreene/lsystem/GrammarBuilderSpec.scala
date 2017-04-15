package ca.jakegreene.lsystem

import org.scalatest.Matchers
import org.scalatest.FlatSpec

class GrammarBuilderSpec extends FlatSpec with Matchers {
  "A GrammarBuilder" should "accept variables" in {
    val builder = new GrammarBuilder()
    val grammar = builder.variables('A', 'B').build()
  }
  
  it should "chain rules" in {
    val builder = new GrammarBuilder()
    val grammar = builder.variables('A', 'B')
      .rule('A' -> "B")
      .rule('B' -> "C")
      .build()
    val result = grammar.produce("A", 2)
    result should equal ("C")
  }
  
  it should "handle contextual rules" in {
    val builder = new GrammarBuilder()
    val grammar = builder.variables('A', 'D')
      .rule('A' -> "B").right("D")
      .rule('D' -> "Z").left("B")
      .rule('A' -> "NEVER") // Should never happen
      .build()
    val result = grammar.produce("AD", 2)
    result should be ("BZ")
  }
  
  it should "handle probabilities" in {
    val builder = new GrammarBuilder()
    val grammar = builder.variables('A')
      .rule('A' -> "B").probability(0.5)
      .rule('A' -> "C").probability(0.5)
      .build()
    val trials = 1000
    val wasBs = for {
      _ <- 1 to trials
      result = grammar.produce("A", 1)  
    } yield result == "B"
    val avgBs = wasBs.count(identity) / trials.toDouble
    avgBs should be >= 0.4
    avgBs should be <= 0.6
  }
  
  it should "handle context and probabilities" in {
    val builder = new GrammarBuilder()
    val grammar = builder.variables('A')
      .rule('A' -> "B").probability(0.5).left("D")
      .rule('A' -> "C").probability(0.5).right("D")
      .rule('A' -> "Z") // should be none of these
      .build()
    val trials = 1000
    val wasBs = for {
      _ <- 1 to trials
      result = grammar.produce("DAD", 1) 
    } yield result == "DBD"
    val avgBs = wasBs.count(identity) / trials.toDouble
    avgBs should be >= 0.40
    avgBs should be <= 0.60
  }
}