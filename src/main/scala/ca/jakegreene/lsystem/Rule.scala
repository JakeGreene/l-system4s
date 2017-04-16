package ca.jakegreene.lsystem

object Rule {
  
  /**
   * A Context Free, Stochastic view of a production rule
   */
  object FreeView {
    def unapply(r: Rule): Option[((Char, String), Double)] = Some(((r.input, r.output), r.weight))
  }
  
  object Free {
    def unapply(r: Rule): Option[((Char, String), Double)] = {
      if (r.left.isEmpty && r.right.isEmpty) Some(((r.input, r.output), r.weight))
      else None
    }
  }
  
  /**
   * Only matches on left-context sensitive production rules
   */
  object Left {
    def unapply(r: Rule): Option[(String, (Char, String), Double)] = {
      if (r.left.isDefined && r.right.isEmpty) Some((r.left.get, (r.input, r.output), r.weight))
      else None
    }
  }
  
  /**
   * Only matches on right-context sensitive production rules
   */
  object Right {
    def unapply(r: Rule): Option[((Char, String), String, Double)] = {
      if (r.left.isEmpty && r.right.isDefined) Some(((r.input, r.output), r.right.get, r.weight))
      else None
    }
  }
  
  /**
   * Only matches on left-and-right-context sensitive production rules
   */
  object Bounded {
    def unapply(r: Rule): Option[(String, (Char, String), String, Double)] = {
      if (r.left.isDefined && r.right.isDefined) Some((r.left.get, (r.input, r.output), r.right.get, r.weight))
      else None
    }
  }
   
  /**
   * Create a Context Free, Deterministic rule
   * 
   * ```
   * val rule = Rule.free('A' -> "B")
   * ```
   */ 
  def free(rule: (Char, String)): Rule = {
    StochasticContextRule(rule._1, rule._2)
  }
  
  /**
   * Create a Context Free, Stochastic rule
   * 
   * ```
   * val rule = Rule.free('A' -> "B", 0.5)
   * ```
   */ 
  def free(rule: (Char, String), weight: Double): Rule = {
    StochasticContextRule(rule._1, rule._2, weight)
  }
  
  /**
   * Create a Context Sensitive, Deterministic rule. Only consider the left side
   * of the variable being changed.
   * 
   * ```
   * val rule = Rule.left("D", 'A' -> "B")
   * ```
   */ 
  def left(left: String, rule: (Char, String)): Rule = {
    StochasticContextRule(rule._1, rule._2, left = Some(left))
  }
  
  /**
   * Create a Context Sensitive, Stochastic rule. Only consider the left side
   * of the variable being changed.
   * 
   * ```
   * val rule = Rule.left("D", 'A' -> "B", 0.5)
   * ```
   */
  def left(left: String, rule: (Char, String), weight: Double): Rule = {
    StochasticContextRule(rule._1, rule._2, weight, left = Some(left))
  }
  
  /**
   * Create a Context Sensitive, Deterministic rule. Only consider the right side
   * of the variable being changed.
   * 
   * ```
   * val rule = Rule.right('A' -> "B", "D")
   * ```
   */ 
  def right(rule: (Char, String), right: String): Rule = {
    StochasticContextRule(rule._1, rule._2, right = Some(right))
  }
  
  /**
   * Create a Context Sensitive, Stochastic rule. Only consider the right side
   * of the variable being changed.
   * 
   * ```
   * val rule = Rule.right('A' -> "B", "D", 0.5)
   * ```
   */ 
  def right(rule: (Char, String), right: String, weight: Double): Rule = {
    StochasticContextRule(rule._1, rule._2, weight, right = Some(right))
  }
  
  /**
   * Create a Context Sensitive, Deterministic rule. Consider both sides of
   * the variable being changed.
   * 
   * ```
   * val rule = Rule.bounded("D", 'A' -> "B", "D")
   * ```
   */ 
  def bounded(left: String, rule: (Char, String), right: String): Rule = {
    StochasticContextRule(rule._1, rule._2, left = Some(left), right = Some(right))
  }
  
  /**
   * Create a Context Sensitive, Stochastic rule. Consider both sides of
   * the variable being changed.
   * 
   * ```
   * val rule = Rule.bounded("D", 'A' -> "B", "D", 0.5)
   * ```
   */ 
  def bounded(left: String, rule: (Char, String), right: String, weight: Double): Rule = {
    StochasticContextRule(rule._1, rule._2, weight, Some(left), Some(right))
  }
  
  def apply(left: Option[String], rule: (Char, String), right: Option[String], weight: Double): Rule = {
    StochasticContextRule(rule._1, rule._2, weight, left, right)
  }
}

sealed trait Rule {
  val input: Char
  val output: String
  val weight: Double
  val left: Option[String]
  val right: Option[String]
}

private case class StochasticContextRule(
    input: Char,
    output: String,
    weight: Double = 1.0,
    left: Option[String] = None,
    right: Option[String] = None
) extends Rule