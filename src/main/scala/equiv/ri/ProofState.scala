package equiv.ri

import equiv.ri.Equation
import equiv.ri.Equation.Side
import equiv.ri.tactics.{EXPANSION, SIMPLIFICATION, DELETION, EQ_DELETION}
import equiv.trs.{Constraint, Rule, Term}
import equiv.trs.Term.Position

object ProofState {

}

case class ProofState(equations: Set[Equation], rules: Set[Rule], flag: Boolean) {
  /** Check if the ProofState has reached a terminal state, i.e. the set of equations is empty */
  def isFinished: Boolean = equations.isEmpty

  /** Check if the proofstate flag is complete (true) */
  def isFlagComplete: Boolean = flag

  /** Change the value of the flag: `true` corresponds to `COMPLETE`, `false` corresponds to `INCOMPLETE` */
  def setFlag(newFlag: Boolean): ProofState = ProofState(equations, rules, newFlag)

  /** Remove the given equation from the set of equations */
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
  def simplifyAll(): ProofState = this.copy(equations = equations.map(_.simplifyCons()))

  /** Given an equation in the proofstate, simplify its constraint and update the proofstate */
  def simplifyEquation(equation: Equation): ProofState =
    assert(equations.contains(equation))
    this.copy(equations = equations - equation + equation.simplifyCons())

  // *************************************************** TACTICS *************************************************** //

  def trySimplification(): ProofState = {
    SIMPLIFICATION.trySimplification(this) match {
      case None => this
      case Some((oldEquation, newEquation)) => 
        println(s"SIMPLIFICATION on:     ${oldEquation.toPrintString()}")
        this.replaceEquationWith(oldEquation, newEquation)
    }
  }

  def tryExpansion(): ProofState = {
    EXPANSION.tryExpansion(this) match
      case None => this
      case Some((oldEquation, newEquations, maybeRule)) =>
        val newPfSt = this.removeEquation(oldEquation).addEquations(newEquations)
        maybeRule match {
          case None => newPfSt
          case Some(rule) => newPfSt.addRule(rule)
        }
  }

  def tryEqDeletion(): ProofState = {
    EQ_DELETION.tryEqDeletion(this)
  }

  def tryDeletion(): ProofState = {
    DELETION.tryDeletion(this)
  }

  override def toString: String = toPrintString(false)

  def toPrintString(colours: Boolean = true): String =
    s"( { ${equations.map(_.toPrintString(colours)).mkString("", ", ", "") } }, { ${rules.map(_.toPrintString(colours)).mkString(sep= ", ")} }, $flag )"

}