package equiv.ri.tactics

import equiv.ri.ProofState
import equiv.ri.Equation
import equiv.utils.Z3
import equiv.utils.TermUtils.getFreshVar
import equiv.trs.Term.{App, Var}
import equiv.trs.Term
import equiv.trs.FunctionSymbol
import equiv.trs.Constraint
import equiv.utils.TermUtils
import equiv.trs.ConstrainedObject
import scala.collection.immutable.LazyList.cons

object EQ_DELETION {
  /** Step-wise try to apply EQ-DELETION on the equations in the given proofstate. After the first possible EQ-DELETION, return the new proofstate.
   * @return [[Some]](proofstate) after one EQ-DELETION on the first possible equation, or [[None]] if no EQ-DELETIONs were possible. */
  def tryEqDeletion(pfSt: ProofState): Option[ProofState] = {
    pfSt.equations.view.flatMap( e1 => tryEqDeletionOnEquation(e1, pfSt) ).headOption
  }

  /** Try to apply EQ-DELETION on the given equation on the outermost possible subterms.
   * @return [[Some]](proofstate) after EQ-DELETION, or [[None]] if EQ-DELETION was not possible. */
  def tryEqDeletionOnEquation(equation: Equation, pfSt: ProofState): Option[ProofState] = {
    tryEqDeletionOnEquationSubtermPairs(equation, getOuterMostTermPairs(equation.left, equation.right, equation.constraintVars), pfSt)
  }

  /** Given an equation and a set of subterm pairs from both sides of the equation, try to apply EQ-DELETION,
   * i.e. add the negation of the conjunction of the equalities of all subterm-pairs to the constraint of the given equation.
   * @example For set of pairs { (s_1, t_1), ..., (s_n, t_n) } the constraint \neg ( s_1 = t_1 /\ ... /\ s_n = t_n ) is added.
   * @param equation The equation that is checked for application.
   * @param subtermPairs The set of subterms to be used for EQ-DELETION
   * @param pfSt The proofstate containing the equation.
   * @param succeedDebug Whether to print on a successful application.
   * @param failDebug Whether to print on a failed application.
   * @return [[Some]](proofstate) after EQ-DELETION, or [[None]] if EQ-DELETION was not possible (i.e. the subtermPairs set was empty) */
  def tryEqDeletionOnEquationSubtermPairs(equation: Equation, subtermPairs: Set[(Term, Term)], pfSt: ProofState, succeedDebug: Boolean = true, failDebug: Boolean = false): Option[ProofState] = {
    if subtermPairs.nonEmpty then
      val newEquation = equation.addConstraint( Constraint( TermUtils.not( ConstrainedObject.foldTerms(subtermPairs.map((t1, t2) => TermUtils.is(t1, t2)) ) ) ) )
      if (succeedDebug) { println(s"EQ-DELETION on ${equation.toPrintString()} gives ${newEquation.toPrintString()}") }
      Some(pfSt.replaceEquationWith(equation, newEquation))
    else
      if (failDebug) { println("EQ-DELETION failed") }
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
    }
    Set()
  }
}
