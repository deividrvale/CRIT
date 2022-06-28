package equiv.trs

import equiv.trs.Term.{App, Substitution}
import equiv.utils.TermUtils
import equiv.trs.Term.Var

case class Rule(left: Term, right: Term, constraints: Set[Constraint]) extends ConstrainedObject(constraints) {
  assert(left.sort == right.sort)

  left match {
    case App(fun, _) => assert(!fun.isTheory)
    case _ => assert(false)
  }

  val vars: Set[Var] = left.vars ++ right.vars ++ constraints.flatMap(_.term.vars)

  val functionSymbols: Set[FunctionSymbol] = left.functionSymbols ++ right.functionSymbols ++ constraints.flatMap(_.term.functionSymbols)

  def rootFunc = left.rootFunc

  def substituteConstraints(substitution: Substitution): Set[Constraint] = {
    constraints.map(_.applySubstitution(substitution))
  }

  /** TODO Check if the addition of `this` rule to the given set of rules is terminating.
   * @return This rule if it is terminating, otherwise None */
  def getIfTerminating(rules: Set[Rule]): Option[Rule] = Some(this)

  override def toString: String = toPrintString(false)

  override def toPrintString(colours: Boolean = true): String = {
    s"${left.toPrintString(colours)} -> ${right.toPrintString(colours)} ${super.toPrintString(colours)}"
  }
}