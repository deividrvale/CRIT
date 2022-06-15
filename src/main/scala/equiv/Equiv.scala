package equiv

import equiv.ri.{Equation, ProofState}
import equiv.ri.Rewrite.*
import equiv.trs.*
import equiv.trs.Temp.*
import equiv.trs.Term.Var
import equiv.trs.Term
import equiv.utils.TermUtils.constraintTrue

object Equiv {
  def main(args: Array[String]): Unit = {
    val eq1 = Equation(termFx, termGy, consXLEZero)
    val pfSt1 = ProofState(Set(eq1), Set(rho1, rho2), true)
    println(pfSt1)
    val pfSt2 = pfSt1.trySimplification()
    println(pfSt2)
  }

  def testApp(rule: Rule, consTerm: ConstrainedTerm): Unit = {
    println(s"Is rule $rule applicable on term $consTerm?: ${consTerm.term.findSubTermInstances(rule.left).nonEmpty}")
  }

}
