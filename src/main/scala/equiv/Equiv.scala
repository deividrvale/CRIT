package equiv

import equiv.ri.{Equation, ProofState}
import equiv.trs.*
import equiv.trs.Temp.*
import equiv.trs.Term.Var
import equiv.trs.Term
import equiv.utils.TermUtils.constraintTrue
import equiv.utils.Z3

object Equiv {
  def main(args: Array[String]): Unit = {
    val eq1: Equation = Equation(termFy, termGy, Set(consVarIntInt("y", ">", 10)))
    val delEq1: Equation = Equation(termFy, termFy, Set())
    val delEq2: Equation = Equation(termFy, termGy, Set(consVarIntInt("y", ">", 1), consVarIntInt("y", "<", 1)))
    val newPfSt: ProofState = ProofState(Set(eq1, delEq2, delEq1), Set(rho1, rho2), true)
    loopSimplify(newPfSt)
  }

  def loopSimplify(pfSt: ProofState): Unit = {
    var newPfSt: ProofState = pfSt
    var i: Int = 0
    while
      i < 3
    do
      i += 1
      println(newPfSt.equations.map(_.toPrintString()))
      newPfSt = newPfSt.trySimplification().simplifyAll().tryEqDeletion().tryDeletion()

    println(newPfSt.equations.map(_.toPrintString()))
  }

  def testApp(rule: Rule, consTerm: ConstrainedTerm): Unit = {
    println(s"Is rule $rule applicable on term $consTerm?: ${consTerm.term.findSubTermInstances(rule.left).nonEmpty}")
  }

}
