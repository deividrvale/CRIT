package equiv.ri.inference_rules

import equiv.ri.Equation.Side
import equiv.ri.{Equation, ProofState}
import equiv.trs.{ConstrainedObject, Constraint, FunctionSymbol, Term}
import equiv.trs.Term.{App, Position, Var}
import equiv.utils.TermUtils.getFreshVar
import equiv.utils.{TermUtils, TheorySymbols, Z3}
import equiv.utils.ListExtension.onNonEmpty

import scala.collection.immutable.LazyList.cons
import scala.language.postfixOps

object EQ_DELETION extends INFERENCE_RULE {
  val name = "EQ-DELETION"

  /** Try to apply EQ-DELETION on the given [[ProofState]].
   * @param pfSt The [[ProofState]] subject to EQ-DELETION.
   * @param equationSelector Function that selects an [[Equation]] from a non-empty list of [[Equation]]s.
   * @param positionsSelector Function that selects a list of [[Position]]s from a non-empty list of [[Position]]s.
   * @return [[Some]]([[pfSt]]) after application of EQ-DELETION if possible, otherwise [[None]]. */
  def tryEQ_DELETION(pfSt: ProofState, equationSelector: List[Equation] => Equation, positionsSelector: (Iterable[Term], List[Position]) => List[Position]): Option[ProofState] = {
    getEQ_DELETIONEquations(pfSt).onNonEmpty(eqs =>
      tryEQ_DELETIONOnEquation(pfSt, equationSelector(eqs), positionsSelector)
    )
  }

  /** Try to apply EQ-DELETION on the given [[ProofState]] and [[Equation]].
   * @param pfSt The [[ProofState]] subject to EQ-DELETION.
   * @param equation The [[Equation]] subject to EQ-DELETION.
   * @param positionsSelector Function that selects a list of [[Position]]s from a non-empty list of [[Position]]s.
   * @return [[Some]]([[pfSt]]) after application of EQ-DELETION if possible, otherwise [[None]]. */
  def tryEQ_DELETIONOnEquation(pfSt: ProofState, equation: Equation, positionsSelector: (Iterable[Term], List[Position]) => List[Position]): Option[ProofState] = {
    getEQ_DELETIONEquationPositions(pfSt, equation).onNonEmpty(positions =>
      Some(
        doEQ_DELETIONOnEquationPositions(pfSt, equation,
          //positionsSelector(List(equation.getSide(Side.Left), equation.getSide(Side.Right)), positions)
          positions
        )
      )
    )
  }

  /** Try to apply EQ-DELETION on the given [[ProofState]].
   * @param pfSt The [[ProofState]] subject to EQ-DELETION.
   * @param equation The [[Equation]] subject to EQ-DELETION.
   * @param positions Non-empty list of [[Position]]s where EQ-DELETION will be performed.
   * @return The [[pfSt]] after application of EQ-DELETION if possible. */
  def doEQ_DELETIONOnEquationPositions(pfSt: ProofState, equation: Equation, positions: List[Position]): ProofState = {
    val newConstraint = Constraint(
      TheorySymbols.notX( ConstrainedObject.termSetToConjunctionTerm(
        positions.map( position => TheorySymbols.eqXY(
          equation.left.subTermAt(position),
          equation.right.subTermAt(position)
        ) ).toSet ) ) )
    pfSt.replaceEquationWith(equation, equation.addConstraint(newConstraint))
  }

  /** @return A [[List]] of [[Equation]]s to which EQ-DELETION can be applied. May be empty. */
  def getEQ_DELETIONEquations(pfSt: ProofState): List[Equation] = {
    pfSt.equations.filter(equation => getEQ_DELETIONEquationPositions(pfSt, equation).nonEmpty).toList
  }

  /** @return A [[List]] of [[Position]]s to which EQ-DELETION can be applied. May be empty. */
  def getEQ_DELETIONEquationPositions(pfSt: ProofState, equation: Equation): List[Position] = {
    getEQ_DELETIONMinimumPositions(equation.left, equation.right, equation.constraintVars).getOrElse(List())
  }

  /** Auxiliary function that recursively computes all positions of two [[Term]]s where EQ-DELETION can be applied.
   * @param leftTerm Initially the left side of the [[Equation]] subject to EQ-DELETION.
   * @param rightTerm Initially the right side of the [[Equation]] subject to EQ-DELETION.
   * @param constraintVars The set of [[Var]]s in the [[Constraint]]s of the [[Equation]] subject to EQ-DELETION.
   * @return [[Some]] [[List]] of [[Position]]s where EQ-DELETION can be applied. May be empty. [[None]] if EQ-DELETION is not possible. */
  def getEQ_DELETIONMinimumPositions(leftTerm: Term, rightTerm: Term, constraintVars: Set[Var]): Option[List[Position]] = {
    if leftTerm.isEqDeletable(constraintVars) && rightTerm.isEqDeletable(constraintVars) then
      return Some(List(List()))
    (leftTerm, rightTerm) match {
      case (Var(x1, _), Var(x2, _)) =>
        if x1 == x2 then
          return Some(List())
      case (App(f1, args1), App(f2, args2)) =>
        if f1 == f2 then
          val recurse = args1.indices.map(id =>
            getEQ_DELETIONMinimumPositions(args1(id), args2(id), constraintVars).map(_.map(id::_))
          )
          val recurse2: Option[List[Position]] = recurse.foldLeft(Some(List()) : Option[List[Position]])((a, b) => mergeMaybeLists(a, b))
          return recurse2
      case _ =>
    }
    None
  }

  def mergeMaybeLists[T](maybeList1: Option[List[T]], maybeList2: Option[List[T]]): Option[List[T]] = {
    (maybeList1, maybeList2) match {
      case (Some(list1), Some(list2)) => Some(list1 ++ list2)
      case _ => None
    }
  }
}
