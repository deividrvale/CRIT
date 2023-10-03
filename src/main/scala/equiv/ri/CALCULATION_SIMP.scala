package equiv.ri

import dotty.tools.dotc.semanticdb.Descriptor.Package
import equiv.utils.ListExtension.onNonEmpty
import equiv.trs.Term.{App, Position, Var}
import equiv.trs.{ConstrainedObject, ConstrainedTerm, Constraint, FunctionSymbol, Term}
import equiv.utils.{TermUtils, Z3}
import equiv.ri.ProofState
import equiv.ri.inference_rules.INFERENCE_RULE

import scala.annotation.tailrec

object CALCULATION_SIMP extends INFERENCE_RULE {
  val name = "CALCULATION (SIMPLIFICATION)"

  def SIMPLIFY_CALC(pfSt: ProofState): ProofState = {
    pfSt.replaceAllEquationWith(simplifyEquations(pfSt.equations))
  }

  def simplifyEquations(equations: Set[Equation]): Set[Equation] = {
    equations.map(equation =>
//      Z3.simplifyEquation(
        removeNonUsedConstraintEquations(equation)
        //)
    )
    //.map(_.simplifyCons()) //.map(Z3.simplifyEquation)
  }

//  def simplify_calc(pfSt: ProofState): ProofState = {
//    var newPfSt = pfSt
//    val newEquations = pfSt.equations.map(_.simplifyCons())
//    if newEquations != pfSt.equations then
//      newPfSt = pfSt.copy(equations = newEquations)
//    newPfSt.replaceAllEquationWith(pfSt.equations.map(Z3.simplifyEquation))
//  }

  // TODO: optimization possible: remove the most constraints possible. e.g. { x >= 1 /\ x <= 1 /\ x = 1 } should become { x = 1 }, instead of { x >= 1 /\ x <= 1 }
  @tailrec
  /** Remove all constraints that are implied by the conjunction of the other constraints.
   * @param remainingConstraints The constraints that have yet to be checked for redundancy. Should initially be the complete set of constraints subject to simplification.
   * @param handledConstraints The constraints that are not implied by the other constraints. Should be empty initially.
   * @return A [[Set]] of [[Constraint]]s, where the redundant constraints are removed. */
  def removeImpliedConstraints(remainingConstraints: Set[Constraint], handledConstraints: Set[Constraint] = Set()): Set[Constraint] = {
    if remainingConstraints.isEmpty then handledConstraints
    else
      val currentConstraint = remainingConstraints.head
      val remainingConstraints2 = remainingConstraints - currentConstraint
      val otherConstraintsConjunct = ConstrainedObject.constraintSetToConjunctionTerm(handledConstraints ++ remainingConstraints2)
      if Z3.implies(otherConstraintsConjunct, currentConstraint.term).contains(true) then
        removeImpliedConstraints(remainingConstraints2, handledConstraints)
      else
        removeImpliedConstraints(remainingConstraints2, handledConstraints + currentConstraint)
  }

  def removeNonUsedConstraintEquations(equation: Equation): Equation = {
    val constraintsToRemove = equation.constraints.filter(constraint =>
      if constraint.term.maybeRootFunc.get.name == TermUtils.equalityFunctionSymbolName then {
        val (left, right) = (constraint.term.subTermAt(List(0)), constraint.term.subTermAt(List(1)))
        /** Return [[True]] if the given variable occurs nowhere else in the equation. */
        def checkIfEquationDoesntContainsVar(eqVar: Var) = {
          eqVar match {
            case Var("w", _) => println(equation.removeConstraint(constraint).vars.mkString(", "))
            case _ =>
          }
          !equation.removeConstraint(constraint).vars.contains(eqVar)
        }
        (left, right) match {
          case (l@Var(_,_), _) => checkIfEquationDoesntContainsVar(l)
          case (_, r@Var(_, _)) => checkIfEquationDoesntContainsVar(r)
          case _ => false
        }
      } else false
    )
    val newEquation = equation.removeConstraints(constraintsToRemove)
    if (constraintsToRemove.nonEmpty) {
      removeNonUsedConstraintEquations(newEquation)
    } else {
      newEquation
    }
  }
}
