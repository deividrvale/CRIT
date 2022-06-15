package equiv.trs

import equiv.utils.TermUtils

case class Constraint(term: Term) {
  assert(term.sort == Sort.Bool)

  override def toString: String = if this == TermUtils.constraintTrue then "" else s" [ $term ]"
}
