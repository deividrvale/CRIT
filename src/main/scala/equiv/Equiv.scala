package equiv

import equiv.ri.{Equation, ProofState}
import equiv.ri.Rewrite.*
import equiv.trs.*
import equiv.trs.Temp.*
import equiv.trs.Term.Var
import equiv.trs.Term
import equiv.utils.TermUtils.constraintTrue
import equiv.utils.Z3

object Equiv {
  def main(args: Array[String]): Unit = {
    val eq1 = Equation(termFy, termGy, Set(consVarIntInt("y", ">", 2)))
    var pfSt = ProofState(Set(eq1), Set(rho1, rho2), true)
    pfSt = pfSt.simplifyAll()
    println(pfSt.toPrintString)
    for (_ <- 0 to 2) {
      pfSt = pfSt.trySimplification()
      pfSt = pfSt.simplifyAll()
      println(pfSt.toPrintString)
    }
  }

  def testApp(rule: Rule, consTerm: ConstrainedTerm): Unit = {
    println(s"Is rule $rule applicable on term $consTerm?: ${consTerm.term.findSubTermInstances(rule.left).nonEmpty}")
  }

}
