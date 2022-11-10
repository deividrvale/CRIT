package equiv.trs

import equiv.trs.Term.{App, Substitution, Var}
import equiv.utils.{TermUtils, TheorySymbols}

case class Constraint(term: Term) {
  assert(term.sort == Sort.Bool)

  def applySubstitution(substitution: Substitution): Constraint = this.copy(term = term.applySubstitution(substitution))

  /** Split a constraint into a set of constraints, on the conjunctions. */
  def split(term2: Term = term): Set[Constraint] = {
    term2 match {
      case App(f, args) =>
        if f.name == TheorySymbols.and.name then
          return args.flatMap(split).toSet
    }
    Set(Constraint(term2))
  }

  /** Try to find the given term in an assignment in the constraint. If there is one, return the first variable that is assigned to it. */
  def maybeFindAssignment(t: Term): Option[Var] = term.maybeFindAssignment(t)

  override def toString: String = toPrintString(false)

  def toPrintString(colours: Boolean = true): String = if this == TheorySymbols.constraintTrue then "" else s" [ ${term.toPrintString(colours)} ]"
}
