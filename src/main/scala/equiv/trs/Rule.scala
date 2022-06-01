package equiv.trs

case class Constraint(term: Term) {
  assert(term.sort == Sort.Bool)
}

case class Rule(left: Term, right: Term, constraint: Constraint) {
  assert(left.sort == right.sort)

  left match {
    case App(fun, _) => assert(!fun.typing.isTheory)
    case _ => assert(false)
  }
}

case class System(theory: String, logic: String, solver: String, signature: Signature, rules: Set[Rule])