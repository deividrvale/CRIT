package equiv.trs

import equiv.trs.Term.{App, Substitution}
import equiv.utils.TheorySymbols

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

  override def toString: String = toPrintString(false)

  def toPrintString(colours: Boolean = true): String = if this == TheorySymbols.constraintTrue then "" else s" [ ${term.toPrintString(colours)} ]"
}
