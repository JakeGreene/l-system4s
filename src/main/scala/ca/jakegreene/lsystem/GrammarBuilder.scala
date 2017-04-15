package ca.jakegreene.lsystem

trait Builder {
  def rule(transform: (Char, String)): Builder
  def build(): Grammar
}

class GrammarBuilder extends Builder { gb =>
  
  var vars = Set[Char]()
  var rules = Set[StochasticContextRule]()
  
  class RuleBuilder(transform: (Char, String)) extends Builder { rb =>
    
    var l: Option[String] = None
    var r: Option[String] = None
    var p: Double = 1.0
    
    def left(l: String): rb.type = {
      rb.l = Some(l)
      rb
    }
    def right(r: String): rb.type = {
      rb.r = Some(r)
      rb
    }
    def weight(p: Double): rb.type = {
      rb.p = p
      rb
    }
    def rule(newTransform: (Char, String)): RuleBuilder = {
      append()
      gb.rule(newTransform)
    }
    def build() = {
      append() 
      gb.build()
    }
    private def append(): Unit = {
      gb.rules += StochasticContextRule(transform._1, transform._2, p, l, r)
    }
  }
  
  def variables(variables: Char*): gb.type = {
    vars = variables.toSet
    gb
  }
  
  def rule(transform: (Char, String)): RuleBuilder = {
    new RuleBuilder(transform)
  }
  
  def build(): Grammar = {
    Grammar.stochasticContextual(vars, rules)
  }
}