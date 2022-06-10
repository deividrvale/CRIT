package equiv.ri.tactics

import equiv.ri.Equation.{Equation, Side}
import equiv.ri.ProofState.ProofState
import equiv.ri.tactics.SIMPLIFICATION.trySimplificationOnEquationSide
import equiv.trs.{Rule, Term}
import equiv.trs.Term.Position

import scala.annotation.tailrec

object SIMPLIFICATION {
  /**
   * @return Updated proofstate together with the equation, side, position and applied rule */
  @tailrec
  def trySimplification(pfSt: ProofState, rules: Set[Rule]): Option[(Equation, Equation)] = {
    if pfSt.equations.isEmpty
    then None
    else
      val oldEquation = pfSt.equations.head
      trySimplificationOnEquation(oldEquation, rules) match {
        case None => trySimplification(pfSt.removeEquation(oldEquation), rules)
        case Some(newEquation) => Some((oldEquation, newEquation))
      }
  }

  def trySimplificationOnEquation(equation: Equation, rules: Set[Rule]): Option[Equation] = {
    trySimplificationOnEquationSide(equation, Side.Left, rules) match {
      case None => trySimplificationOnEquationSide(equation, Side.Right, rules)
      case x => x
    }
  }

  def trySimplificationOnEquationSide(equation: Equation, side: Side, rules: Set[Rule]): Option[Equation] = {
    trySimplificationOnEquationSide(equation.getSide(side), rules) match {
      case None => None
      case Some(term) => side match {
        case Side.Left => Some(Equation(term, equation.right, equation.cons))
        case Side.Right => Some(Equation(equation.left, term, equation.cons))
      }
    }
  }

  @tailrec
  def trySimplificationOnEquationSide(term: Term, rules: Set[Rule]): Option[Term] = {
    if rules.isEmpty
    then None
    else
      val rule = rules.head
      trySimplificationOnEquationSideWithRule(term, rule) match {
        case None => trySimplificationOnEquationSide(term, rules - rule)
        case x => x
      }
  }

  def trySimplificationOnEquationSideWithRule(term: Term, rule: Rule): Option[Term] = {
    term.findSubTerms(t => t.instanceOf(rule.left)) match {
      case List() => None
      case (term, position, substitution)::_ => Some(term.rewriteAtPos(position, rule.right, substitution))
    }
  }

}
