package ca.jakegreene.lsystem

import org.scalatest.Matchers
import org.scalatest.FlatSpec

class StochasticGrammarSpec extends FlatSpec with Matchers {
  "A StochasticGrammar" should "randomly select weighted rules" in {
    val aToB = Rule('A', "B", 0.5)
    val aToC = Rule('A', "C", 0.5)
    val grammar = Grammar.stochastic(Set('A', 'B', 'C'), Set(aToB, aToC))
    val axiomSize = 1000
    val axiom = "A" * axiomSize
    val trials = 100
    val bPercents = for {
      _ <- 0 to trials
    } yield grammar.produce(axiom, 1).count(_ == 'B') / axiomSize.toDouble
    
    val averageBPortion = bPercents.sum / trials 
    averageBPortion should be > 0.4
    averageBPortion should be < 0.6
  }
  
  it should "infer constants" in {
    val aToB = Rule('A', "B", 1.0)
    val grammar = Grammar.stochastic(Set('A', 'B', 'C'), Set(aToB))
    val result = grammar.produce("AC", 1)
    result should be ("BC")
  }
  
  it should "iteratively produce" in {
    val aToB = Rule('A', "B", 1.0)
    val bToC = Rule('B', "C", 1.0)
    val cToD = Rule('C', "D", 1.0)
    val grammar = Grammar.stochastic(Set('A', 'B', 'C'), Set(aToB, bToC, cToD))
    val result = grammar.produce("ABCD", 2)
    result should be ("CDDD")
  }
}