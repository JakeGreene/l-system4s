package ca.jakegreene.lsystem

object ContextRule {
  def free(input: Char, output: String) = ContextRule(input, output)
  def left(input: Char, output: String, left: String) = ContextRule(input, output, left = Some(left))
  def right(input: Char, output: String, right: String) = ContextRule(input, output, right = Some(right))
  def bound(input: Char, output: String, left: String, right: String) = ContextRule(input, output, Some(left), Some(right))
}

case class ContextRule(input: Char, output: String, left: Option[String] = None, right: Option[String] = None)

case class ContextSensitiveGrammar(variables: Set[Char], rules: Set[ContextRule]) extends Grammar {
  
  val ruleSets = rules.groupBy(_.input)
  val contextualRules = ruleSets.mapValues { 
    _.filter {
      case ContextRule(_, _, None, None) => false
      case _ => true
    }
  }.filter {
    case (_, rules) if rules.isEmpty => false
    case _ => true
  }

  val freeRules = ruleSets.mapValues {
    _.filter {
      case ContextRule(_, _, None, None) => true
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
    result = result.zipWithIndex.flatMap {
      case (v, i) if contextualRules.isDefinedAt(v) =>
        def leftSlice(size: Int) = result.slice(i - size, i)
        def rightSlice(size: Int) = result.slice(i + 1, i + size + 1)
        val rules = contextualRules(v)
        val matchedRules = rules.filter {
          case ContextRule(_, output, Some(l), Some(r)) =>
            val left = leftSlice(l.length)
            val right = rightSlice(r.length)
            left == l && right == r
          case ContextRule(_, output, None, Some(r)) =>
            val right = rightSlice(r.length)
            right == r
          case ContextRule(_, output, Some(l), None) =>
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
          val context = result.slice(0, i) + "\\033[1;31m" + v + "\\033[1;31m" + result.slice(i + 1, result.length)
          throw new IllegalArgumentException(
            s"Ambiguous rules. Rules $matchedRules all match for variable $v in context $context"
          )
        }
      case (v, _) if freeRules.isDefinedAt(v) =>
        val rule = freeRules(v)
        rule.output
      case (constant, _) =>
        constant.toString
    }.mkString
    result
  }
}