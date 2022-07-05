package equiv.ri.tactics

import equiv.ri.ProofState
import equiv.ri.Equation
import equiv.utils.Z3
import equiv.utils.TermUtils.{constraintFalse, constraintTrue, getFreshVar}
import equiv.trs.Term.{App, Position, Var}
import equiv.trs.Term
import equiv.trs.FunctionSymbol
import equiv.trs.Constraint
import equiv.utils.TermUtils
import equiv.trs.ConstrainedObject

import scala.collection.immutable.LazyList.cons

object EQ_DELETION {
  val name = "EQ-DELETION"

  /** Step-wise try to apply EQ-DELETION on the equations in the given proofstate. After the first possible EQ-DELETION, return the new proofstate.
   * @return [[Some]](proofstate) after one EQ-DELETION on the first possible equation, or [[None]] if no EQ-DELETIONs were possible. */
  def tryEqDeletion(pfSt: ProofState): Option[ProofState] = {
    pfSt.equations.view.flatMap( e1 => tryEqDeletionOnEquation(e1, pfSt) ).headOption
  }

  /** Try to apply EQ-DELETION on the given equation on the outermost possible subterms.
   * @return [[Some]](proofstate) after EQ-DELETION, or [[None]] if EQ-DELETION was not possible. */
  def tryEqDeletionOnEquation(equation: Equation, pfSt: ProofState): Option[ProofState] = {
    doEqDeletionOnEquationSubtermPairs(equation, getOuterMostTermPairs(equation.left, equation.right, equation.constraintVars), pfSt)
  }

  /** For every given position, try to get the subterms at these positions in the given equation and perform EQ-DELETION with these subterm pairs.
   * @param equation The equation that is checked for application.
   * @param positions The set of positions for subterms to be used for EQ-DELETION
   * @param pfSt The proofstate containing the equation.
   * @param succeedDebug Whether to print on a successful application.
   * @param failDebug Whether to print on a failed application.
   * @return [[Some]](proofstate) after EQ-DELETION, or [[None]] if the subtermPairs set was empty */
  def tryEqDeletionOnEquationOnPositions(equation: Equation, positions: Set[Position], pfSt: ProofState, succeedDebug: Boolean = true, failDebug: Boolean = false): Option[ProofState] = {
    val subtermPairs = positions.map( pos =>
      tryToGetSubtermPairAtPosition(equation.left, equation.right, pos, equation.constraintVars)
        .getOrElse( {
          if (failDebug) { println(s"$name failed") }
          return None
        } ) )
    doEqDeletionOnEquationSubtermPairs(equation, subtermPairs, pfSt, succeedDebug, failDebug)
  }

  /** Given an equation and a set of subterm pairs from both sides of the equation, apply EQ-DELETION,
   * i.e. add the negation of the conjunction of the equalities of all subterm-pairs to the constraint of the given equation.
   * @example For set of pairs { (s_1, t_1), ..., (s_n, t_n) } the constraint \neg ( s_1 = t_1 /\ ... /\ s_n = t_n ) is added.
   * @param equation The equation that is checked for application.
   * @param subtermPairs The set of subterms to be used for EQ-DELETION
   * @param pfSt The proofstate containing the equation.
   * @param succeedDebug Whether to print on a successful application.
   * @param failDebug Whether to print on a failed application.
   * @return [[Some]](proofstate) after EQ-DELETION, or [[None]] if the subtermPairs set was empty */
  def doEqDeletionOnEquationSubtermPairs(equation: Equation, subtermPairs: Set[(Term, Term)], pfSt: ProofState, succeedDebug: Boolean = true, failDebug: Boolean = false): Option[ProofState] = {
    if subtermPairs.nonEmpty then
      val newEquation = equation.addConstraint( Constraint( TermUtils.not( ConstrainedObject.foldTerms(subtermPairs.map((t1, t2) => TermUtils.is(t1, t2)) ) ) ) )
      if (succeedDebug) { println(s"$name on ${equation.toPrintString()} gives ${newEquation.toPrintString()}") }
      Some(pfSt.replaceEquationWith(equation, newEquation))
    else
      if (failDebug) { println(s"$name failed") }
      None
  }

  /** Get the outermost pairs of terms for which EQ-DELETION is possible.
   * @return A set of pairs of [[Term]]s */
  def getOuterMostTermPairs(term1: Term, term2: Term, constraintVars: Set[Var]): Set[(Term, Term)] = {
    if term1.isEqDeletable(constraintVars) && term2.isEqDeletable(constraintVars) then
      return Set((term1, term2))
    else (term1, term2) match {
      case (App(f1, args1), App(f2, args2)) =>
        if f1 == f2 then
          return args1.zip(args2).flatMap( (t1, t2) => getOuterMostTermPairs(t1, t2, constraintVars) ).toSet
      case _ =>
    }
    Set()
  }

  /** Recursively try to get a pair of subterms: one from `leftTerm`, one from `rightTerm`, at the same position, as long as they are eq-deletable.
   * @param leftTerm The [[Term]] corresponding initially to the [[Left]] side of the equation.
   * @param rightTerm The [[Term]] corresponding initially to the [[Right]] side of the equation.
   * @param position The [[Position]] to get the subterms from in both [[Term]]s
   * @param constraintVars The set of variables in the constraint of the equation.
   * @return [[Some]] pair of terms if EQ-DELETION is possible here, otherwise [[None]] */
  def tryToGetSubtermPairAtPosition(leftTerm: Term, rightTerm: Term, position: Position, constraintVars: Set[Var]): Option[(Term, Term)] = {
    position match {
      case List() =>
        if leftTerm.isEqDeletable(constraintVars) && rightTerm.isEqDeletable(constraintVars) then
          return Some((leftTerm, rightTerm))
      case x::xs => (leftTerm, rightTerm) match {
        case (App(f1, args1), App(f2, args2)) =>
          if f1 == f2 then
            return tryToGetSubtermPairAtPosition(args1(x), args2(x), xs, constraintVars)
        case _ =>
      }
    }
    None
  }

  /** Get the subterm pairs and position of every pair of subterms that is subject to EQ-DELETION
   * @param leftTerm The [[Term]] corresponding initially to the [[Left]] side of the equation.
   * @param rightTerm The [[Term]] corresponding initially to the [[Right]] side of the equation.
   * @param constraintVars The set of variables in the constraint of the equation.
   * @return A [[Set]] of all found positions with subterm pairs subject to EQ-DELETION. */
  def getAllPossibleEqDeletionPositions(leftTerm: Term, rightTerm: Term, constraintVars: Set[Var]): Set[(Position, (Term, Term))] = {
    var positions: Set[(Position, (Term, Term))] = Set()
    if leftTerm.isEqDeletable(constraintVars) && rightTerm.isEqDeletable(constraintVars) then
      positions += (List(), (leftTerm, rightTerm))
    (leftTerm, rightTerm) match {
      case (App(f1, args1), App(f2, args2)) =>
        if f1 == f2 then
          for i <- args1.indices do {
            positions ++= getAllPossibleEqDeletionPositions(args1(i), args2(i), constraintVars).map( (pos, terms) => (i::pos, terms) )
          }
      case _ =>
    }
    positions
  }
}
