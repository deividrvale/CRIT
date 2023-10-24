package equiv.trs

import equiv.ri.Equation
import equiv.ri.Equation.Side
import equiv.trs.Term.{App, Substitution}
import equiv.trs.Term.Var
import equiv.trs.parsing.QuasiRule
import equiv.utils.TermUtils

case class Rule(left: Term, right: Term, constraints: Set[Constraint]) extends ConstrainedObject(constraints) {
  assert(left.sort == right.sort)

  left match {
    case App(fun, _) => assert(!fun.isTheory)
    case _ => assert(false)
  }

  val vars: Set[Var] = left.vars ++ right.vars ++ constraintVars

  val functionSymbols: Set[FunctionSymbol] = left.functionSymbols ++ right.functionSymbols ++ constraints.flatMap(_.term.functionSymbols)

  val asEquation: Equation = Equation(left, right, constraints)
  val getReverseEquation: Equation = Equation(right, left, constraints)

  /** Set of logic variables: variables in the constraint together with ('fresh') variables in the right side that do not occur in the left side. */
  val logicVars: Set[Var] = constraintVars ++ (right.vars -- left.vars)

  def rootFunc: Option[FunctionSymbol] = left.maybeRootFunc

  def substituteConstraints(substitution: Substitution): Set[Constraint] = {
    constraints.map(_.applySubstitution(substitution))
  }

  /** Substitute all occurrences of `matchTerm` by `replacementTerm`
   * @param matchTerm       Sub-term that will be replaced
   * @param replacementTerm Term that will replace occurrences of `matchTerm`
   * @return The current rule with all occurrences of [[matchTerm]] replaced by [[replacementTerm]]. */
  def substituteAll(matchTerm: Term, replacementTerm: Term): Rule = {
    Rule(this.left.substituteAll(matchTerm, replacementTerm),
      this.right.substituteAll(matchTerm, replacementTerm),
      constraints.map(_.substituteAll(matchTerm, replacementTerm)))
  }

  /** Rename every occurrence of a variable from the given [[List]] into a fresh variable.
   * @param variables A [[List]] of variables that we wish to rename when encountered in the current term
   * @return [[this]] [[Term]] with every occurrence of a variable in the given list renamed to a fresh variable. */
  def renameVarOccurrences(variables: Set[Var]): Rule = {
    var rule = this
    for (variable <- variables) do
      var currentVariable = variable
      var replace = false
      while rule.vars.contains(currentVariable) do
        currentVariable = TermUtils.getFreshVar(variable.sort)
        replace = true
      if replace then rule = rule.substituteAll(variable, currentVariable)
    rule
  }

  /**
   * Get the variables and values in the LHS and RHS of the rule, in the order of occurrence when reading the rule from left to right.
   * @example `f(x, 2) -> g(z) [ x = 4 + x_1 /\ y = z + 1 ]` gives `[x, 2, z]`.
   * @return A [[List]] of [[Term]]s.
   */
  def getRuleLRHSVarsValsInOrder: List[Term] = {
    this.left.getVarsValsInOrder ++ this.right.getVarsValsInOrder
  }

  /** TODO Check if the addition of `this` rule to the given set of rules is terminating.
   * @return This rule if it is terminating, otherwise None */
  def getIfTerminating(rules: Set[Rule]): Option[Rule] = Some(this)

  override def toString: String = toPrintString(false)

  override def toPrintString(colours: Boolean = true): String = {
    s"${left.toPrintString(colours)} -> ${right.toPrintString(colours)} ${super.toPrintString(colours)}"
  }
}