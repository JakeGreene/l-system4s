package ca.jakegreene.lsystem

object Main extends App {
  val grammar = Grammar.deterministic(Set('A', 'B'), Map(('A' -> "AB"), ('B' -> "A")))
  println(grammar.produce("A", 4))
}