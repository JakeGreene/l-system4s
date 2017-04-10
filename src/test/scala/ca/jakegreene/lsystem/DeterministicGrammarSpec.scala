package ca.jakegreene.lsystem

import org.scalatest.Matchers
import org.scalatest.FlatSpec

class DeterministicGrammarSpec extends FlatSpec with Matchers {
  "A DeterministicGrammer" should "transform variables based on rules" in {
    val grammar = Grammar.deterministic(Set('A', 'B'), Map(('A' -> "AB"), ('B' -> "A")))
    val result = grammar.produce("A", 4)
    result should be ("ABAABABA")
  }
  
  it should "infer constants" in {
    val grammar = Grammar.deterministic(Set('A', 'B'), Map(('A' -> "AB")))
    val result = grammar.produce("A", 4)
    result should be ("ABBBB")
  }
}