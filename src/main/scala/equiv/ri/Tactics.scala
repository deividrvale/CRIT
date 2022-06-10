package equiv.ri

import equiv.ri.Equation.Equation
import equiv.trs.Rule

object Tactics {

  def simplification(equation: Equation, rule: Rule): Unit = {
    val posList = equation.left.findSubTermInstances(rule.left)
  }
}
