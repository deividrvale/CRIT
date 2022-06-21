package equiv.trs

import equiv.trs.Term.Substitution
import equiv.utils.TermUtils

case class Constraint(term: Term) {
  assert(term.sort == Sort.Bool)

  def applySubstitution(substitution: Substitution): Constraint = this.copy(term = term.applySubstitution(substitution))

  override def toString: String = toPrintString(false)

  def toPrintString(colours: Boolean = true): String = if this == TermUtils.constraintTrue then "" else s" [ ${term.toPrintString(colours)} ]"
}
