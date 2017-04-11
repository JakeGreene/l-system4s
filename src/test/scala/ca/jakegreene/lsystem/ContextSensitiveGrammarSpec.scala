package ca.jakegreene.lsystem

import org.scalatest.Matchers
import org.scalatest.FlatSpec

class ContextSensitiveGrammarSpec extends FlatSpec with Matchers {
  "A ContextSensitiveGrammar" should "follow free rules" in {
    val aToB = ContextRule.free('A', "B")
    val grammar = Grammar.contextual(Set('A'), Set(aToB))
    val result = grammar.produce("A", 1)
    result should be ("B")
  } 
  
  it should "infer constants" in {
    val aToB = ContextRule.free('A', "B")
    val grammar = Grammar.contextual(Set('A'), Set(aToB))
    val result = grammar.produce("AC", 1)
    result should be ("BC")
  }
  
  it should "iteratibely produce" in {
    val aToB = ContextRule.free('A', "B")
    val bToC = ContextRule.free('B', "C")
    val cToD = ContextRule.free('C', "D")
    val grammar = Grammar.contextual(Set('A'), Set(aToB, bToC, cToD))
    val result = grammar.produce("ABCD", 2)
    result should be ("CDDD")
  }
  
  it should "follow left sensitive rules" in {
    val aToB = ContextRule.left('A', "B", "C")
    val grammar = Grammar.contextual(Set('A'), Set(aToB))
    val result = grammar.produce("ACA", 1)
    result should be ("ACB")
  }
  
  it should "follow right sensitive rules" in {
    val aToB = ContextRule.right('A', "B", "C")
    val grammar = Grammar.contextual(Set('A'), Set(aToB))
    val result = grammar.produce("ACA", 1)
    result should be ("BCA")
  }
  
  it should "follow bounded rules" in {
    val aToB = ContextRule.bound('A', "B", "D", "D")
    val grammar = Grammar.contextual(Set('A'), Set(aToB))
    val result = grammar.produce("ADADA")
    result should be ("ADBDA")
  }
  
  it should "follow left contextual rules before free ones" in {
    val aToB = ContextRule.free('A', "B")
    val aToZWhenC = ContextRule.left('A', "Z", "C")
    val grammar = Grammar.contextual(Set('A'), Set(aToB, aToZWhenC))
    val result = grammar.produce("CA", 1)
    result should be ("CZ")
  }
  
  it should "follow right contextual rules before free ones" in {
    val aToB = ContextRule.free('A', "B")
    val aToZWhenC = ContextRule.right('A', "Z", "C")
    val grammar = Grammar.contextual(Set('A'), Set(aToB, aToZWhenC))
    val result = grammar.produce("AC", 1)
    result should be ("ZC")
  }
  
  it should "follow bounded rules before free ones" in {
    val aToB = ContextRule.free('A', "B")
    val aToZWhenC = ContextRule.bound('A', "Z", "C", "D")
    val grammar = Grammar.contextual(Set('A'), Set(aToB, aToZWhenC))
    val result = grammar.produce("CAD", 1)
    result should be ("CZD")
  }
  
  it should "apply all valid rules at the same time during a step" in {
    val aToB = ContextRule.free('A', "B")
    val bToC = ContextRule.left('B', "C", "B")
    val grammar = Grammar.contextual(Set('A', 'B'), Set(aToB, bToC))
    val result = grammar.produce("AB", 1)
    result should be ("BB")
  }
  
  it should "find ambiguous context rules" in {
    val aToB = ContextRule.left('A', "B", "D")
    val aToC = ContextRule.right('A', "C", "D")
    val grammar = Grammar.contextual(Set('A'), Set(aToB, aToC))
    intercept[IllegalArgumentException] {
      val result = grammar.produce("DAD", 1)  
    }
  }
  
  it should "enforce one free rule per variable" in {
    val aToB = ContextRule.free('A', "B")
    val aToC = ContextRule.free('A', "C")
    val grammar = Grammar.contextual(Set('A'), Set(aToB, aToC))
    intercept[IllegalArgumentException] {
      val result = grammar.produce("A", 1)
    }
  }
}