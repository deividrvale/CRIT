package equiv.trs

case class Constraint(term: Term) {
  assert(term.sort == Sort.Bool)

  override def toString: String = s"[ $term ]"
}
