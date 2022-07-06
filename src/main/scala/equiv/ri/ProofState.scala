package equiv.ri

import equiv.ri.Equation
import equiv.ri.Equation.Side
import equiv.ri.tactics.{EXPANSION, SIMPLIFICATION, DELETION, EQ_DELETION}
import equiv.trs.{Constraint, Rule, Term}
import equiv.trs.Term.Position
import equiv.trs.FunctionSymbol
import equiv.trs.Term.Var
import equiv.ri.tactics.CONSTRUCTOR
import equiv.ri.tactics.COMPLETENESS

object ProofState {

}

case class ProofState(equations: Set[Equation], rules: Set[Rule], private val flag: Boolean) {
  var isFalse = false

  /** The set of all function symbols occurring in the proofstate */
  val functionSymbols: Set[FunctionSymbol] = rules.flatMap( _.functionSymbols )

  /** The set of defined symbols of the given ruleset, i.e. the set of all function symbols that are the root of the left-hand side of a rule.
   * @example The defined symbols in rule set `{ f(x) -> g(x - 1) [x > 0], h(x) -> return(x) }` are `f` and `h` */
  val definedSymbols: Set[FunctionSymbol] = rules.flatMap( r => r.rootFunc )
  
  /** The set of all constructors in the proofstate: function symbols that are non-theory or values. */
  val constructors: Set[FunctionSymbol] = functionSymbols.filter(_.isConstructor(definedSymbols))

  val vars: Set[Var] = equations.flatMap(_.vars) ++ rules.flatMap(_.vars)

  /** Check if the ProofState has reached a terminal state, i.e. the set of equations is empty */
  def isFinished: Boolean = equations.isEmpty

  /** Get the value of the flag: `true` corresponds to `COMPLETE`, `false` corresponds to `INCOMPLETE` */
  def getFlag: Boolean = flag

  /** Change the value of the flag: `true` corresponds to `COMPLETE`, `false` corresponds to `INCOMPLETE` 
    * Also change the `COMPLETENESS.lastcompleteProofStateEquations` value to the current proofstate's equations if the flag is changed from true (COMPLETE) to false (INCOMPLETE) */
  def setFlag(newFlag: Boolean): ProofState = { if flag && !newFlag then COMPLETENESS.lastCompleteProofStateEquations = Some(equations) ; ProofState(equations, rules, newFlag) }

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

  /** Add the given rule to the `rules` set. */
  def addRule(rule: Rule): ProofState = ProofState(equations, rules + rule, flag)

  /** If the given parameter is of the form `Some(r)`, then return the current proofstate with the rule `r`, otherwise return the current proofstate. */
  def maybeAddRule(rule: Option[Rule]): ProofState = rule.map(r => return this.addRule(r)).getOrElse(return this)

  /** Simplify all equation constraints */
  def simplifyAll(): ProofState = this.copy(equations = equations.map(_.simplifyCons()))

  /** Given an equation in the proofstate, simplify its constraint and update the proofstate */
  def simplifyEquation(equation: Equation): ProofState = {
    assert(equations.contains(equation))
    this.copy(equations = equations - equation + equation.simplifyCons())
  }

  override def toString: String = toPrintString(false)

  def toPrintString(colours: Boolean = true): String =
    s"( E = { ${equations.map(_.toPrintString(colours)).mkString("", ", ", "") } }, \n  H = { ${rules.map(_.toPrintString(colours)).mkString(sep= ", ")} },\n  flag = $flag )"

}