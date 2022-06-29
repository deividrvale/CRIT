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
  
  def tryEqDeletion(pfSt: ProofState): Option[ProofState] = {
    pfSt.equations
      .view
      .flatMap( e1 => tryEqDeletionOnEquation(e1, pfSt).map( e2 => pfSt.replaceEquationWith(e1, e2) ) )
      .headOption
  }

  def tryEqDeletionOnEquation(equation: Equation, pfSt: ProofState): Option[Equation] = {
    val constraints = getEqualities(equation.left, equation.right, equation.constraintVars, pfSt.definedSymbols)
    if constraints.isEmpty then None else {
      val newEquation = equation.copy(constraints = equation.constraints + Constraint(TermUtils.not(ConstrainedObject.foldTerms(constraints))))
      println(s"EQ-DELETION on ${equation.toPrintString()} gives ${newEquation.toPrintString()}.")
      Some(newEquation)
    }
  }

  def getEqualities(term1: Term, term2: Term, constrainedVars: Set[Var], definedSymbols: Set[FunctionSymbol]): Set[Term] = {
    if term1.isEqDeletable(constrainedVars) && term2.isEqDeletable(constrainedVars) then 
      Set(TermUtils.is(term1, term2))
    else (term1, term2) match {
      case (App(f1, args1), App(f2, args2)) => {
        if f1 == f2 then
          args1.zip(args2).flatMap( (t1, t2) => getEqualities(t1, t2, constrainedVars, definedSymbols) ).toSet
        else Set()
      }
      case _ => Set()
    }
  }
}
