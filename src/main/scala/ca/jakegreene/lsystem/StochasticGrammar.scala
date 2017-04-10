package ca.jakegreene.lsystem

case class Rule(input: Char, output: String, probability: Double)

case class StochasticGrammar(variables: Set[Char], rules: Set[Rule]) extends Grammar {
  
  val ruleSets = rules.groupBy(r => r.input).mapValues { case rules => 
    val orderedRules = rules.toList
    /*
     * Seq.inits is biggest init (i.e. all elements) to smallest init (i.e. empty)
     * but we need smallest (1 element) to largest in order to calculate a CDF
     */  
    val inits = orderedRules.inits.toList.reverse.filterNot(_.isEmpty)
    val cdf = inits.map(_.map(_.probability).sum)
    orderedRules.zip(cdf)
  }
  
  def produce(axiom: String, steps: Int = 100): String = {
    var result = axiom
    for (i <- 0 to steps) {
      result = result.flatMap {
        case v if ruleSets.isDefinedAt(v) =>
          val rulesWithCDF = ruleSets(v)
          /* 
           * Randomly select an element by using the cumulative distribution function
           * of the weights. The first element with a cumulative weight greater than
           * or equal to a random number chosen from [0, cdfTotal) is the item to be
           * selected.
           */ 
          val cdfTotal = rulesWithCDF.map(_._2).max
          val rnd = scala.util.Random.nextDouble * cdfTotal
          val (rule, _) = rulesWithCDF.find { case (rule, cdfAtPoint) => cdfAtPoint >= rnd }.get
          rule.output
        case constant => constant.toString
      }
    }
    result
  }
}