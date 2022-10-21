package equiv.trs

import equiv.ri.Equation

trait Query
case class QueryEquivalence(equation: Equation) extends Query

