package equiv.ri

import equiv.ri.Equation
import equiv.ri.Equation.Side
import equiv.ri.tactics.SIMPLIFICATION
import equiv.trs.{Constraint, Rule, Term}
import equiv.trs.Term.Position

object ProofState {

}

case class ProofState(equations: Set[Equation], rules: Set[Rule], flag: Boolean) {
  /** Check if the proofstate flag is complete (true) */
  def isComplete: Boolean = flag

  /** Change the value of the flag: `true` corresponds to `COMPLETE`, `false` corresponds to `INCOMPLETE` */
  def setFlag(newFlag: Boolean): ProofState = ProofState(equations, rules, newFlag)

  def removeEquation(equation: Equation): ProofState = ProofState(equations - equation, rules, flag)

  /** Add a single equation to the proofstate */
  def addEquation(equation: Equation): ProofState = ProofState(equations + equation, rules, flag)

  /** Add a set of equations to the proofstate */
  def addEquations(newEquations: Set[Equation]): ProofState = ProofState(equations ++ newEquations, rules, flag)

  /** Remove an equation from the equations set and add a new one
   * @param oldEquation Equation to be removed
   * @param newEquation Equation to be added */
  def replaceEquationWith(oldEquation: Equation, newEquation: Equation): ProofState =
    ProofState(equations - oldEquation + newEquation, rules, flag)

  def addRule(rule: Rule): ProofState = ProofState(equations, rules + rule, flag)

  /** Simplify all equation constraints */
  def simplifyAll(): ProofState = this.copy(equations = equations.map(_.simplifyM()))

  /** Given an equation in the proofstate, simplify its constraint and update the proofstate */
  def simplifyEquation(equation: Equation): ProofState =
    assert(equations.contains(equation))
    this.copy(equations = equations - equation + equation.simplifyM())

  // *************************************************** TACTICS *************************************************** //

  def trySimplification(): ProofState =
    SIMPLIFICATION.trySimplification(this, rules) match {
      case None => this
      case Some((oldEquation, newEquation)) => this.replaceEquationWith(oldEquation, newEquation)
    }

  def EXPANSION(side: Side, equation: Equation): ProofState = {
    ProofState(equations ++ equation.getExpd(side), rules ++ equation.maybeHypothesis(side, rules), flag)
  }

  override def toString: String =
    s"( { ${equations.mkString(sep = ", ") } }, { ${rules.mkString(sep= ", ")}, $flag )"

  def toPrintString: String =
    s"( { ${equations.map(_.toPrintString).mkString("", ", ", "") } }, { ${rules.map(_.toPrintString).mkString(sep= ", ")} }, $flag )"

}