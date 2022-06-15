package equiv.ri.tactics

import equiv.ri.{Equation, ProofState}
import equiv.ri.Equation.Side
import equiv.ri.tactics.SIMPLIFICATION.trySimplificationOnTerm
import equiv.trs.{Rule, Term}
import equiv.trs.Term.Position

import scala.annotation.tailrec

object SIMPLIFICATION {
  /**
   * @return Updated proofstate together with the equation, side, position and applied rule */
  def trySimplification(pfSt: ProofState, rules: Set[Rule]): Option[(Equation, Equation)] = {
    pfSt.equations.view.flatMap { oldEquation =>
      trySimplificationOnEquation(oldEquation, rules).map((oldEquation,_))
    }.headOption
  }

  def trySimplificationOnEquation(equation: Equation, rules: Set[Rule]): Option[Equation] = {
    List(Side.Left, Side.Right).view.flatMap{ side =>
      trySimplificationOnEquationSide(equation, side, rules)
    }.headOption
  }

  def trySimplificationOnEquationSide(equation: Equation, side: Side, rules: Set[Rule]): Option[Equation] = {
    trySimplificationOnTerm(equation.getSide(side), rules).map(equation.withSide(side,_))
  }

  def trySimplificationOnTerm(term: Term, rules: Set[Rule]): Option[Term] = {
    rules.view.flatMap { rule => trySimplificationOnTermWithRule(term, rule) }.headOption
  }

  def trySimplificationOnTermWithRule(term: Term, rule: Rule): Option[Term] = {
    term.findSubTerms(t => t.instanceOf(rule.left)) match {
      case List() => None
      case (_, position, substitution)::_ => Some(term.rewriteAtPos(position, rule.right, substitution))
    }
  }

}
