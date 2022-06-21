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
    val eq1 = Equation(termFy, termGy, Set(consVarIntInt("y", ">", 3), consVarIntInt2("y", ">", 3)))
    val newPfSt = ProofState(Set(eq1), Set(rho1, rho2), true)
    loopSimplify(newPfSt)
  }

  def loopSimplify(pfSt: ProofState): Unit = {
    var newPfSt = pfSt
    var i = 0
    while
      i < 4
    do
      i += 1
      println(newPfSt.toPrintString())
      newPfSt = newPfSt.trySimplification().simplifyAll()

    println(newPfSt.toPrintString())
  }

  def testApp(rule: Rule, consTerm: ConstrainedTerm): Unit = {
    println(s"Is rule $rule applicable on term $consTerm?: ${consTerm.term.findSubTermInstances(rule.left).nonEmpty}")
  }

}
