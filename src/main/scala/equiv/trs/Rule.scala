package equiv.trs

import equiv.trs.Term.App

case class Rule(left: Term, right: Term, constraint: Option[Constraint]) {
  assert(left.sort == right.sort)

  left match {
    case App(fun, _) => assert(!fun.typing.isTheory)
    case _ => assert(false)
  }

  override def toString: String = {
    s"$left -> $right ${constraint.map(_.toString).getOrElse("")}"
  }
}