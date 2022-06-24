package equiv.ri

import equiv.ri.Equation
import equiv.ri.Equation.Side
import equiv.ri.tactics.{EXPANSION, SIMPLIFICATION, DELETION, EQ_DELETION}
import equiv.trs.{Constraint, Rule, Term}
import equiv.trs.Term.Position
import equiv.trs.FunctionSymbol
import equiv.trs.Term.Var

object ProofState {

}

case class ProofState(equations: Set[Equation], rules: Set[Rule], flag: Boolean) {
  /** The set of defined symbols of the given ruleset, i.e. the set of all function symbols that are the root of the left-hand side of a rule.
   * @example The defined symbols in rule set `{ f(x) -> g(x - 1) [x > 0], h(x) -> return(x) }` are `f` and `h` */
  val definedSymbols: Set[FunctionSymbol] = rules.flatMap( r => r.rootFunc )

  def getVars(): Set[Var] = equations.flatMap(_.vars) ++ rules.flatMap(_.vars)

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
  def simplifyEquation(equation: Equation): ProofState = {
    assert(equations.contains(equation))
    this.copy(equations = equations - equation + equation.simplifyCons())
  }

  // *************************************************** TACTICS *************************************************** //

  def trySimplification(): Option[ProofState] = {
    SIMPLIFICATION.trySimplification(this).map( (oldEquation, newEquation) => 
        this.replaceEquationWith(oldEquation, newEquation) )
  }

  def tryExpansion(): Option[ProofState] = {
    EXPANSION.tryExpansion(this).map( (oldEquation, newEquations, maybeRule) =>
      val newPfSt = this.removeEquation(oldEquation).addEquations(newEquations)
      maybeRule match {
        case None => newPfSt
        case Some(rule) => newPfSt.addRule(rule)
      } )
  }

  def tryEqDeletion(): Option[ProofState] = {
    EQ_DELETION.tryEqDeletion(this)
  }

  def tryDeletion(): Option[ProofState] = {
    DELETION.tryDeletion(this)
  }

  override def toString: String = toPrintString(false)

  def toPrintString(colours: Boolean = true): String =
    s"( E = { ${equations.map(_.toPrintString(colours)).mkString("", ", ", "") } }, \n  H = { ${rules.map(_.toPrintString(colours)).mkString(sep= ", ")} },\n  flag = $flag )"

}