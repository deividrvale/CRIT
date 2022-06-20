package equiv.trs

import equiv.trs.Term.App
import equiv.utils.TermUtils

case class Rule(left: Term, right: Term, constraints: Set[Constraint]) extends ConstrainedObject(constraints) {
  assert(left.sort == right.sort)

  left match {
    case App(fun, _) => assert(!fun.typing.isTheory)
    case _ => assert(false)
  }

  /** TODO Check if addition of the current rule to the given terminating set keeps the set terminating */
  def isTerminating(rules: Set[Rule]): Boolean = true

  override def toString: String = {
    s"$left -> $right $printConstraints"
  }
}