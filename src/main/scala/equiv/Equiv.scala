package equiv

import equiv.ri.ProofState.*
import equiv.ri.Rewrite.{rewriteAtPos, *}
import equiv.ri.Equation.{Equation, Side}
import equiv.trs.*
import equiv.trs.Temp.*
import equiv.trs.Term
import equiv.trs.Core.boolTrue

object Equiv {
  def main(args: Array[String]): Unit = {
    println("Hello")

    val pfst1 = ProofState(Set(Equation(termFx, termGx, Constraint(boolTrue))), Set(rho1, rho2), true)
    println(pfst1)
    val pfst2 = pfst1.SIMPLIFICATION(pfst1.equations.head, Side.Left, rho1, List())
    println(pfst2)
    val pfst3 = pfst2.DELETION(pfst2.equations.head)
    println(pfst3)
  }

  def testApp(rule: Rule, consTerm: ConstrainedTerm): Unit = {
    println(s"Is rule $rule applicable on term $consTerm?: ${consTerm.term.findSubTermInstances(rule.left).nonEmpty}")
  }

}
