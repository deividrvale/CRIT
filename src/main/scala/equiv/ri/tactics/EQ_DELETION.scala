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

object EQ_DELETION {
  
  def tryEqDeletion(pfSt: ProofState): Option[ProofState] = {
    pfSt.equations
      .view
      .flatMap( e1 => tryEqDeletionOnEquation(e1, pfSt).map( e2 => pfSt.replaceEquationWith(e1, e2) ) )
      .headOption
  }

  def tryEqDeletionOnEquation(equation: Equation, pfSt: ProofState): Option[Equation] = {
    maybeReplace(equation.left, equation.right, equation.constraintVars, pfSt.definedSymbols).map( (t1, t2, cs) => Equation(t1, t2, equation.constraints + Constraint(TermUtils.not(ConstrainedObject.foldTerms(cs)))) )
  }

  def maybeReplace(term1: Term, term2: Term, constrainedVars: Set[Var], definedSymbols: Set[FunctionSymbol]): Option[(Term, Term, Set[Term])] = {
    if term1.isEqDeletable(constrainedVars) && term2.isEqDeletable(constrainedVars) then 
      Some((getFreshVar(term1.sort), getFreshVar(term2.sort), Set(TermUtils.is(term1, term2))))
    else (term1, term2) match {
      case (App(f1, args1), App(f2, args2)) => {
        if f1 == f2 then
            var amountOfSubtermsNotReplaced = 0
            var constraints: Set[Term] = Set()
            val (args11, args21): (List[Term], List[Term]) = 
              args1.zip(args2).map( (t1, t2) => maybeReplace(t1, t2, constrainedVars, definedSymbols).map( (t11, t21, cons) => { constraints ++= cons ; (t11, t21) } ).getOrElse( { amountOfSubtermsNotReplaced += 1; (t1, t2) } ) ).unzip
            if amountOfSubtermsNotReplaced == args1.length then None else Some((App(f1, args11), App(f2, args21), constraints))
        else None
      }
      case _ => None
    }
  }
}
