package ca.jakegreene.lsystem

case class ContextSensitiveGrammar(variables: Set[Char], rules: Set[Rule]) extends Grammar {

  val ruleSets = rules.groupBy(_.input)
  val contextualRules = ruleSets.mapValues {
    _.filter {
      case Rule.Free(_, _) => false
      case _ => true
    }
  }.filter {
    case (_, rules) if rules.isEmpty => false
    case _ => true
  }

  val freeRules = ruleSets.mapValues {
    _.filter {
      case Rule.Free(_, _) => true
      case _ => false
    }
  }.filter {
    case (_, rules) if rules.nonEmpty => true
    case _ => false
  }.mapValues { rules =>
    // There can only be one context-free rule per variable
    require(rules.size == 1)
    rules.head
  }

  def produce(axiom: String, steps: Int = 100): String = {
    var result = axiom
    for (step <- 0 until steps) {
      result = result.zipWithIndex.flatMap {
        case (v, i) if contextualRules.isDefinedAt(v) =>
          def leftSlice(size: Int) = result.slice(i - size, i)
          def rightSlice(size: Int) = result.slice(i + 1, i + size + 1)
          val rules = contextualRules(v)
          val matchedRules = rules.filter {
            case Rule.Bounded(l, (_, output), r, _) =>
              val left = leftSlice(l.length)
              val right = rightSlice(r.length)
              left == l && right == r
            case Rule.Right((_, output), r, _) =>
              val right = rightSlice(r.length)
              right == r
            case Rule.Left(l, (_, output), _) =>
              val left = leftSlice(l.length)
              left == l
            case _ => false
          }
          if (matchedRules.isEmpty) {
            // Default to the identity A -> A
            v.toString
          } else if (matchedRules.size == 1) {
            matchedRules.head.output
          } else {
            throw new IllegalArgumentException(
              s"""Ambiguous rules. Rules $matchedRules all match for variable $v in context ${result} at step $step""")
          }
        case (v, _) if freeRules.isDefinedAt(v) =>
          val rule = freeRules(v)
          rule.output
        case (constant, _) =>
          constant.toString
      }.mkString
    }
    result
  }
}