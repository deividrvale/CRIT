package equiv.trs

import equiv.trs.ConstrainedObject.removeImpliedConstraints
import equiv.utils.{TermUtils, Z3}

import javax.swing.SpringLayout.Constraints

object ConstrainedObject {
  def removeImpliedConstraints(constraints: Set[Constraint]): Set[Constraint] = {
    if constraints.size <= 1 then return constraints
    val constraint_1 = constraints.head
    val constraints_2 = constraints - constraint_1
    for (constraint_2 <- constraints) {
      // If there exists another constraint that implies the given constraint, remove it
      if constraint_2 != constraint_1 && Z3.constraintImplication(constraint_2.term, constraint_1.term) then
        return removeImpliedConstraints(constraints_2)
    }
    removeImpliedConstraints(constraints_2) + constraint_1
  }
}

trait ConstrainedObject(constraints: Set[Constraint]) {
  /** Removes all constraints that are implied by another constraint in the set */
  def simplify(): Set[Constraint] = removeImpliedConstraints(constraints)

  /** Combine all constraints into a single constraint with conjunctions
   * @example `{ [x > 0], [x < 2] }` becomes `[x > 0 /\ x < 2]` */
  def getConstrainsConjunctAsConstraint: Constraint =
    Constraint(constraints.foldRight(TermUtils.boolTrue)((c1, c2) => TermUtils.and(c1.term, c2)))

  /** Combine all constraints into a single term with conjunctions
   * @example `{ [x > 0], [x < 2] }` becomes `x > 0 /\ x < 2` */
  def getConstrainsConjunctAsTerm: Term =
    constraints.foldRight(TermUtils.boolTrue)((c1, c2) => TermUtils.and(c1.term, c2))

  /** Print the set of constraints as a conjunction. */
  override def toString: String = toPrintString(false)

  /** Prints the set of constraints as a conjunction, possibly with colours */
  def toPrintString(colours: Boolean = true): String =
    s"[ ${constraints.map(_.term.toPrintString(colours)).mkString(sep = " /\\ ")} ]"
}
