package ca.jakegreene.lsystem

case class StochasticContextSensitiveGrammar(variables: Set[Char], rules: Set[Rule]) extends Grammar {

  val ruleSets = rules.groupBy(_.input)
  val contextualRules = ruleSets.mapValues { rules =>
    val contextual = rules.filter {
      case Rule.Free(_, _) => false
      case _ => true
    }
    val orderedRules = contextual.toList
    /*
     * Seq.inits is biggest init (i.e. all elements) to smallest init (i.e. empty)
     * but we need smallest (1 element) to largest in order to calculate a CDF
     */  
    val inits = orderedRules.inits.toList.reverse.filterNot(_.isEmpty)
    val cdf = inits.map(_.map(_.weight).sum)
    orderedRules.zip(cdf)
  }.filter {
    case (_, rules) if rules.isEmpty => false
    case _ => true
  }

  val freeRules = ruleSets.mapValues { rules =>
    val contextual = rules.filter {
      case Rule.Free(_, _) => true
      case _ => false
    }
    val orderedRules = contextual.toList
    /*
     * Seq.inits is biggest init (i.e. all elements) to smallest init (i.e. empty)
     * but we need smallest (1 element) to largest in order to calculate a CDF
     */  
    val inits = orderedRules.inits.toList.reverse.filterNot(_.isEmpty)
    val cdf = inits.map(_.map(_.weight).sum)
    orderedRules.zip(cdf)
  }.filter { case (_, rules) =>
    rules.nonEmpty
  }

  def produce(axiom: String, steps: Int = 100): String = {
    var result = axiom
    for (step <- 0 until steps) {
      result = result.zipWithIndex.flatMap {
        case (v, i) if contextualRules.isDefinedAt(v) =>
          applyContextual(result, v, i)
        case (v, _) if freeRules.isDefinedAt(v) =>
          applyFree(v)
        case (constant, _) =>
          constant.toString
      }.mkString
    }
    result
  }

  private def applyContextual(context: String, variable: Char, index: Int): String = {
    def leftSlice(size: Int) = context.slice(index - size, index)
    def rightSlice(size: Int) = context.slice(index + 1, index + size + 1)
    val rules = contextualRules(variable)
    val matchedRules = rules.filter {
      case (Rule.Bounded(l, (_, output), r, _), _) =>
        val left = leftSlice(l.length)
        val right = rightSlice(r.length)
        left == l && right == r
      case (Rule.Right((_, output), r, _), _) =>
        val right = rightSlice(r.length)
        right == r
      case (Rule.Left(l, (_, output),_), _) =>
        val left = leftSlice(l.length)
        left == l
      case _ => false
    }
    if (matchedRules.isEmpty) {
      // The free rules might have something for this variable
      applyFree(variable)
    } else if (matchedRules.size == 1) {
      matchedRules.head._1.output
    } else {
      /* 
       * Randomly select an element by using the cumulative distribution function
       * of the weights. The first element with a cumulative weight greater than
       * or equal to a random number chosen from [0, cdfTotal) is the item to be
       * selected.
       */
      val cdfTotal = matchedRules.map(_._2).max
      val rnd = scala.util.Random.nextDouble * cdfTotal
      val (rule, _) = matchedRules.find { case (rule, cdfAtPoint) => cdfAtPoint >= rnd }.get
      rule.output
    }
  }

  def applyFree(variable: Char): String = {
    val maybeRulesWithCDF = freeRules.get(variable)
    maybeRulesWithCDF.fold(variable.toString){ rulesWithCDF =>
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
    }
  }
}