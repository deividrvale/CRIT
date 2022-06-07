package equiv.trs

case class ConstrainedTerm(term: Term, constraint: Constraint) {
  override def toString: String = {
    s"${term.toString} ${constraint.toString}"
  }
}