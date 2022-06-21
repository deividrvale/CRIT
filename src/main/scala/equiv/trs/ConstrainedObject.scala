package equiv.trs

import equiv.utils.{TermUtils, Z3}

trait ConstrainedObject(constraints: Set[Constraint]) {
  /** Removes all constraints that are implied by another constraint in the set */
  def simplify(): Set[Constraint] =
     constraints.filter(c1 => !constraints.exists(c2 => c1 != c2 && Z3.constraintImplication(c2.term, c1.term)))

  /** Combine all constraints into a single constraint with conjunctions
   * @example `{ [x > 0], [x < 2] }` becomes `[x > 0 /\ x < 2]` */
  def getConstrainsConjunctAsConstraint: Constraint =
    Constraint(constraints.foldRight(TermUtils.boolTrue)((c1, c2) => TermUtils.and(c1.term, c2)))

  /** Combine all constraints into a single term with conjunctions
   * @example `{ [x > 0], [x < 2] }` becomes `x > 0 /\ x < 2` */
  def getConstrainsConjunctAsTerm: Term =
    constraints.foldRight(TermUtils.boolTrue)((c1, c2) => TermUtils.and(c1.term, c2))

  /** Print the set of constraints as a conjunction. */
  override def toString: String =
    s"[ ${constraints.mkString(sep = " /\\ ")} ]"

  def toPrintString: String =
    s"[ ${constraints.map(_.term.toPrintString).mkString(sep = " /\\ ")} ]"
}
