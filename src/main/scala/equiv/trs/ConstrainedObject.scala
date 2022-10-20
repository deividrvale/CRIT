package equiv.trs

import equiv.ri.Simplify
import equiv.utils.{PrintUtils, TermUtils, TheorySymbols, Z3}

import javax.swing.SpringLayout.Constraints
import equiv.trs.Term.Var

import scala.collection.immutable.LazyList.cons

object ConstrainedObject {
  /** 'Fold' the given set of constraints into a single term with conjunctions. */
  def constraintSetToConjunctionTerm(constraints: Set[Constraint]): Term = {
    termSetToConjunctionTerm(constraints.map(_.term))
  }

  /** 'Fold' the given set of terms into a single term with conjunctions. */
  def termSetToConjunctionTerm(constraints: Set[Term]): Term = {
    if constraints.size > 1 then
      constraints.tail.foldLeft(constraints.head)((c1, c2) => TheorySymbols.andXY(c1, c2))
    else if constraints.nonEmpty then
      constraints.head
    else
      TheorySymbols.boolTrue
  }

  /** 'Fold' the given set of constraints into a single constraint with conjunctions. */
  def constraintSetToConjunctionConstraint(constraints: Set[Constraint]): Constraint = {
    Constraint(constraintSetToConjunctionTerm(constraints))
  }

  /** 'Fold' the given set of terms into a single constraint with conjunctions. */
  def termSetToConjunctionConstraint(constraints: Set[Term]): Constraint = {
    Constraint(termSetToConjunctionTerm(constraints))
  }
}

trait ConstrainedObject(constraints: Set[Constraint]) {
  /** Removes all constraints that are implied by another constraint in the set */
  def simplify(): Set[Constraint] = Simplify.removeImpliedConstraints(constraints)

  /** Get the variables in all the constraints */
  def constraintVars: Set[Var] = constraints.flatMap(_.term.vars)

  /** Combine all constraints into a single constraint with conjunctions
   * @example `{ [x > 0], [x < 2] }` becomes `[x > 0 /\ x < 2]` */
  def getConstrainsConjunctAsConstraint: Constraint = {
    ConstrainedObject.constraintSetToConjunctionConstraint(constraints)
  }

  /** Combine all constraints into a single term with conjunctions
   * @example `{ [x > 0], [x < 2] }` becomes `x > 0 /\ x < 2` */
  def getConstrainsConjunctAsTerm: Term =
    ConstrainedObject.constraintSetToConjunctionTerm(constraints)

  /** Print the set of constraints as a conjunction. */
  override def toString: String = toPrintString(false)

  /** Prints the set of constraints as a conjunction, possibly with colours */
  def toPrintString(colours: Boolean = true): String =
    if constraints.isEmpty then "" else
    s"[ ${constraints.map(_.term.toPrintString(colours)).mkString(sep = s" ${PrintUtils.functionSymbolPrintStrings(TheorySymbols.and.name)} ")} ]"
}
