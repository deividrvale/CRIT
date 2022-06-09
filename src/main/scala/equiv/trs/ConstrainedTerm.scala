package equiv.trs

import equiv.trs.Term.Var

case class ConstrainedTerm(term: Term, constraint: Constraint) {
  /** @return A list of variables that occur in this constrained term */
  def vars(ct: ConstrainedTerm): Set[Var] = ct.term.vars ++ ct.constraint.term.vars

  override def toString: String = {
    s"${term.toString} ${constraint.toString}"
  }
}