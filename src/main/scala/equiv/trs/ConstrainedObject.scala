package equiv.trs

import equiv.utils.TermUtils

class ConstrainedObject(constraints: Set[Constraint]) {
  /** TODO Simplifies the constraints */
  def simplify: ConstrainedObject = this

  /** Combine all constraints into a single constraint with conjunctions
   * @example `{ [x > 0], [x < 2] }` becomes `[x > 0 /\ x < 2]` */
  def getConstrainsConjunctAsConstraint: Constraint =
    Constraint(constraints.foldRight(TermUtils.boolTrue)((c1, c2) => TermUtils.and(c1.term, c2)))

  /** Combine all constraints into a single term with conjunctions
   * @example `{ [x > 0], [x < 2] }` becomes `x > 0 /\ x < 2` */
  def getConstrainsConjunctAsTerm: Term =
    constraints.foldRight(TermUtils.boolTrue)((c1, c2) => TermUtils.and(c1.term, c2))

  /** Print the set of constraints as a conjunction. */
  def printConstraints: String =
    s"[ ${constraints.foldRight("")((c1, c2) => c1.term.toString ++ " /\\ " ++ c2).dropRight(4)} ]"
}
