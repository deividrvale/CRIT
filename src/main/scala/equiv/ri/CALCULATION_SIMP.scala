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
      Z3.simplifyEquation(substituteVarEqualitiesInConstraints(equation))
    )
  }

  def simplifyEquation(equation: Equation): Equation = {
    Z3.simplifyEquation(substituteVarEqualitiesInConstraints(equation))
  }

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

  /**
   * Remove all equality-constraints from the given equation that are unnecessary.
   * In particular, constraints of the form "v = ...", where v is a variable that does not occur anywhere else in the equation.
   * Keep doing this (using recursion) until there are no more of these constraints.
 *
   * @param equation Equation to potentially remove constraints from
   * @return New equation with unnecessary constraints removed.
   */
  @tailrec
  @deprecated
  def removeNonUsedConstraintEquations(equation: Equation): Equation = {
    val constraintsToRemove = equation.constraints.filter(constraint =>
      if constraint.term.maybeRootFunc.get.name == TermUtils.equalityFunctionSymbolName then {
        val (left, right) = (constraint.term.subTermAt(List(0)), constraint.term.subTermAt(List(1)))
        /** Return [[True]] if the given variable occurs nowhere else in the equation. */
        def checkIfEquationDoesntContainVar(eqVar: Var) = !equation.removeConstraint(constraint).vars.contains(eqVar)
        (left, right) match {
          case (l@Var(_,_), _) => checkIfEquationDoesntContainVar(l)
          case (_, r@Var(_, _)) => checkIfEquationDoesntContainVar(r)
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

  /***
   * Find constraints of the form "v = ...", where v is a variable that does not occur in the left- or right-side of the equation. Then replace v by ... in all constraints.
   * @param equation
   * @return
   */
  def substituteVarEqualitiesInConstraints(equation: Equation): Equation = {
    // Step 1: get all constraints of the form "v = ...", with v not in Var(left) U Var(right)
    val varEqualityConstraints: Set[Constraint] = equation.constraints.filter(constraint =>
      if constraint.term.maybeRootFunc.get.name == TermUtils.equalityFunctionSymbolName then {
        val (left, right) = (constraint.term.subTermAt(List(0)), constraint.term.subTermAt(List(1)))
        def checkIfEquationLRDoesntContainVar(eqVar: Var) = !(equation.left.vars ++ equation.right.vars).contains(eqVar)
        (left, right) match {
          case (l@Var(_, _), _) => checkIfEquationLRDoesntContainVar(l)
          case (_, r@Var(_, _)) => checkIfEquationLRDoesntContainVar(r)
          case _ => false
        }
      } else false
    )
    var varReplacements: Set[(Var, Term)] = varEqualityConstraints.map(constraint =>
      (constraint.term.subTermAt(List(0)), constraint.term.subTermAt(List(1))) match {
        case (l@Var(_, _), r) => (l, r)
        case (l, r@Var(_, _)) => (r, l)
        case _ => throw Error("Hmm there is no variable, something is wrong in the code.")
    })

    // Step 2: replace all occurrences of the variables from step 1
    var constraints = equation.constraints -- varEqualityConstraints
    while varReplacements.nonEmpty do {
      val varReplacement@(v, t) = varReplacements.head
      constraints = constraints.map(c => Constraint(c.term.applySubstitution(Map(v -> t))))
      varReplacements = (varReplacements - varReplacement).map((vr, term) => (vr, term.applySubstitution(Map(v -> t))))
    }

    equation.replaceAllConstraints(constraints)
  }
}
