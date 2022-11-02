package equiv.ri

import equiv.utils.ListExtension.onNonEmpty
import equiv.trs.Term.{App, Position, Var}
import equiv.trs.{ConstrainedObject, ConstrainedTerm, Constraint, FunctionSymbol, Term}
import equiv.utils.{TermUtils, Z3}

import scala.annotation.tailrec

object CALCULATION {
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

  def trySubtermVarReplacement(pfSt: ProofState, equationSelector: List[Equation] => Equation): Option[ProofState] = {
    getSubtermVarReplacementEquations(pfSt).onNonEmpty(
      eqs => doSubtermVarReplacementOnEquation(pfSt, equationSelector(eqs))
    )
  }

  def doSubtermVarReplacementOnEquation(pfSt: ProofState, equation: Equation): Option[ProofState] = {
    val positions = getEquationSubtermVarReplacementPositions(equation) // TODO: remove duplicate work
    if positions.nonEmpty then
      Some(doSubtermVarReplacementOnEquationPositions(pfSt, equation, positions))
    else None
  }

  def doSubtermVarReplacementOnEquationPositions(pfSt: ProofState, equation: Equation, positions: List[Position]): ProofState = {
    var updatedEquation: ConstrainedTerm = equation.toConstrainedTerm
    positions.foreach(
      pos =>
        val subterm: Term = updatedEquation.term.subTermAt(pos)
        maybeGetFirstVar(updatedEquation.constraints) match {
          case Some(x@Var(_, _)) => updatedEquation = updatedEquation.substituteAtPos(pos, x)
          case None =>
            val freshVar = TermUtils.getFreshVar(subterm.sort)
            val newConstraint = Constraint(App(TermUtils.getEqualityFunctionSymbol(subterm.sort), List(freshVar, subterm)))
            updatedEquation = updatedEquation.substituteAtPos(pos, freshVar).addConstraint(newConstraint)
        }
    )
    updatedEquation match {
      case ConstrainedTerm(App(_, List(l, r)), cons) => pfSt.replaceEquationWith(equation, Equation(l, r, cons))
      case _ => throw Error("Something went wrong")
    }
  }

  @tailrec
  def maybeGetFirstVar(constraints: Iterable[Constraint]): Option[Var] = {
    val maybeVar: Option[Var] = constraints.headOption.flatMap(_.maybeVar)
    if maybeVar.nonEmpty then maybeVar else maybeGetFirstVar(constraints.drop(1))
  }

  def getSubtermVarReplacementEquations(pfSt: ProofState): List[Equation] = {
    pfSt.equations.filter(getEquationSubtermVarReplacementPositions(_).nonEmpty).toList
  }

  /** Get a list of positions of biggest subterms that are calculations containing variables, but not only variables or only theory symbols.
   * IMPORTANT: the positions are for the equation interpreted as a single term, with ~~ a fresh symbol as root!! */
  def getEquationSubtermVarReplacementPositions(equation: Equation): List[Position] = {
    getEquationSubtermVarReplacementPositionsAux(equation.left).map(0::_) ++
      getEquationSubtermVarReplacementPositionsAux(equation.right).map(1::_)
  }

  /** Get a list of [[Position]]s of subterms we can replace with (fresh) variables. */
  def getEquationSubtermVarReplacementPositionsAux(term: Term): List[Position] = {
    term match {
      case Var(_, _) => List()
      case t@App(_, args) => if t.isCalculationContainingVariables() then
        List(List())
      else args.zipWithIndex.flatMap((t, i) => getEquationSubtermVarReplacementPositionsAux(t).map(i::_))
    }
  }

}
