package ca.jakegreene.lsystem

import org.scalatest.Matchers
import org.scalatest.FlatSpec

class StochasticContextSensitiveGrammarSpec extends FlatSpec with Matchers {
  "A StochasticContextSensitiveGrammar" should  "follow free rules" in {
    val aToB = Rule.free('A' -> "B", 1.0)
    val grammar = Grammar.stochasticContextual(Set('A'), Set(aToB))
    val result = grammar.produce("A", 1)
    result should be ("B")
  } 
  
  it should "infer constants" in {
    val aToB = Rule.free('A' -> "B", 1.0)
    val grammar = Grammar.stochasticContextual(Set('A'), Set(aToB))
    val result = grammar.produce("AC", 1)
    result should be ("BC")
  }
  
  it should "iteratibely produce" in {
    val aToB = Rule.free('A' -> "B", 1.0)
    val bToC = Rule.free('B' -> "C", 1.0)
    val cToD = Rule.free('C' -> "D", 1.0)
    val grammar = Grammar.stochasticContextual(Set('A'), Set(aToB, bToC, cToD))
    val result = grammar.produce("ABCD", 2)
    result should be ("CDDD")
  }
  
  it should "follow left sensitive rules" in {
    val aToB = Rule.left("C", 'A' -> "B", 1.0)
    val grammar = Grammar.stochasticContextual(Set('A'), Set(aToB))
    val result = grammar.produce("ACA", 1)
    result should be ("ACB")
  }
  
  it should "follow right sensitive rules" in {
    val aToB = Rule.right('A' -> "B", "C", 1.0)
    val grammar = Grammar.stochasticContextual(Set('A'), Set(aToB))
    val result = grammar.produce("ACA", 1)
    result should be ("BCA")
  }
  
  it should "follow bounded rules" in {
    val aToB = Rule.bounded("D", 'A' -> "B", "D", 1.0)
    val grammar = Grammar.stochasticContextual(Set('A'), Set(aToB))
    val result = grammar.produce("ADADA")
    result should be ("ADBDA")
  }
  
  it should "always follow left contextual rules before free ones" in {
    val aToB = Rule.free('A' -> "B", 1.0)
    val aToZWhenC = Rule.left("C", 'A' -> "Z", 1.0)
    val zToYWhenC = Rule.left("C", 'Z' -> "Y", 1.0)
    val grammar = Grammar.stochasticContextual(Set('A', 'Z'), Set(aToB, aToZWhenC, zToYWhenC))
    val result = grammar.produce("CA" * 100, 2)
    result should be ("CY" * 100)
  }
  
  it should "always follow right contextual rules before free ones" in {
    val aToB = Rule.free('A' -> "B", 1.0)
    val aToZWhenC = Rule.right('A' -> "Z", "C", 1.0)
    val zToYWhenC = Rule.right('Z' -> "Y", "C", 1.0)
    val grammar = Grammar.stochasticContextual(Set('A', 'Z'), Set(aToB, aToZWhenC, zToYWhenC))
    val result = grammar.produce("AC" * 100, 2)
    result should be ("YC" * 100)
  }
  
  it should "always follow bounded rules before free ones" in {
    val aToB = Rule.free('A' -> "B", 1.0)
    val aToZWhenCD = Rule.bounded("C", 'A' -> "Z", "D", 1.0)
    val zToYWhenCD = Rule.bounded("C", 'Z' -> "Y", "D", 1.0)
    val grammar = Grammar.stochasticContextual(Set('A', 'Z'), Set(aToB, aToZWhenCD, zToYWhenCD))
    val result = grammar.produce("CAD", 2)
    result should be ("CYD")
  }
  
  it should "fall back to free rules when no contextual apply" in {
    val aToB = Rule.free('A' -> "B", 1.0)
    val aToCWhenD = Rule.left("D", 'A' -> "C", 1.0)
    val grammar = Grammar.stochasticContextual(Set('A'), Set(aToB, aToCWhenD))
    val result = grammar.produce("A", 1)
    result should be ("B")
  }
  
  it should "apply all valid rules at the same time during a step" in {
    val aToB = Rule.free('A' -> "B", 1.0)
    val bToC = Rule.left("B", 'B' -> "C", 1.0)
    val grammar = Grammar.stochasticContextual(Set('A', 'B'), Set(aToB, bToC))
    val result = grammar.produce("AB", 1)
    result should be ("BB")
  }
  
  it should "randomly select between conflicting context rules" in {
    val aToB = Rule.left("D", 'A' -> "B", 0.5)
    val aToC = Rule.right('A' -> "C", "D", 0.5)
    val grammar = Grammar.stochasticContextual(Set('A', 'B'), Set(aToB, aToC))
    val trials = 100
    val numAs = 10
    val bShares = for {
      i <- 1 to trials
      val result = grammar.produce("DAD" * numAs, 1)
      val bShare = result.count(_ == 'B') / numAs.toDouble
    } yield bShare
    val averageBShare = bShares.sum / trials
    averageBShare should be > (0.4)
    averageBShare should be < (0.6)
  }
  
  it should "randomly select between conflicting free rules" in {
    val aToB = Rule.free('A' -> "B", 0.5)
    val aToC = Rule.free('A' -> "C", 0.5)
    val grammar = Grammar.stochasticContextual(Set('A', 'B'), Set(aToB, aToC))
    val trials = 100
    val numAs = 10
    val bShares = for {
      i <- 1 to trials
      val result = grammar.produce("A" * numAs, 1)
      val bShare = result.count(_ == 'B') / numAs.toDouble
    } yield bShare
    val averageBShare = bShares.sum / trials
    averageBShare should be > (0.4)
    averageBShare should be < (0.6)
  }
}