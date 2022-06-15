package equiv

import equiv.ri.{Equation, ProofState}
import equiv.ri.Rewrite.{rewriteAtPos, *}
import equiv.trs.*
import equiv.trs.Temp.*
import equiv.trs.Term.Var
import equiv.trs.Term
import equiv.trs.Core.boolTrue

object Equiv {
  def main(args: Array[String]): Unit = {
    val eq1 = Equation(termGFy, termGy, Constraint(boolTrue))
    val pfSt1 = ProofState(Set(eq1), Set(rho1, rho2), true)
    println(pfSt1)
    val pfSt2 = pfSt1.trySimplification()
    println(pfSt2)
  }

  def testApp(rule: Rule, consTerm: ConstrainedTerm): Unit = {
    println(s"Is rule $rule applicable on term $consTerm?: ${consTerm.term.findSubTermInstances(rule.left).nonEmpty}")
  }

}
