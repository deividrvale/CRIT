package equiv.trs

import equiv.trs.ConstrainedObject.removeImpliedConstraints
import equiv.utils.{TermUtils, Z3}

import javax.swing.SpringLayout.Constraints
import equiv.trs.Term.Var
import scala.collection.immutable.Stream.Cons
import scala.collection.immutable.LazyList.cons

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

  def foldConstraints(constraints: Set[Constraint]): Term = {
    foldTerms(constraints.map(_.term))
  }

  def foldTerms(constraints: Set[Term]): Term = {
    if constraints.size > 1 then
      constraints.tail.foldLeft(constraints.head)((c1, c2) => TermUtils.and(c1, c2))
    else if !constraints.isEmpty then
      constraints.head
    else
      TermUtils.boolTrue
  }

  def constraintSetToConjunct(constraints: Set[Constraint]): Constraint = {
    Constraint(foldConstraints(constraints))
  }

  def termSetToConstraintConjunct(constraints: Set[Term]): Constraint = {
    Constraint(foldTerms(constraints))
  }
}

trait ConstrainedObject(constraints: Set[Constraint]) {
  /** Removes all constraints that are implied by another constraint in the set */
  def simplify(): Set[Constraint] = removeImpliedConstraints(constraints)

  /** Get the variables in all the constraints */
  def constraintVars: Set[Var] = constraints.flatMap(_.term.vars)

  /** Combine all constraints into a single constraint with conjunctions
   * @example `{ [x > 0], [x < 2] }` becomes `[x > 0 /\ x < 2]` */
  def getConstrainsConjunctAsConstraint: Constraint = {
    ConstrainedObject.constraintSetToConjunct(constraints)
  }

  /** Combine all constraints into a single term with conjunctions
   * @example `{ [x > 0], [x < 2] }` becomes `x > 0 /\ x < 2` */
  def getConstrainsConjunctAsTerm: Term =
    ConstrainedObject.foldConstraints(constraints)

  /** Print the set of constraints as a conjunction. */
  override def toString: String = toPrintString(false)

  /** Prints the set of constraints as a conjunction, possibly with colours */
  def toPrintString(colours: Boolean = true): String =
    if constraints.isEmpty then "" else
    s"[ ${constraints.map(_.term.toPrintString(colours)).mkString(sep = " /\\ ")} ]"
}
