package equiv.ri.tactics

import equiv.ri.{Equation, ProofState}
import equiv.ri.Equation.Side
import equiv.ri.tactics.SIMPLIFICATION.trySimplificationOnTerm
import equiv.trs.{Constraint, Rule, Term}
import equiv.trs.Term.Position
import equiv.utils.Z3

import scala.annotation.tailrec

object SIMPLIFICATION {
  /**
   * @return Either a pair with as first argument an old equation and as second argument the updated equation, or None if no simplification is possible */
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
    trySimplificationOnTerm(equation.getSide(side), equation.constraint, rules).map(equation.withSide(side,_))
  }

  def trySimplificationOnTerm(term: Term, constraint: Constraint, rules: Set[Rule]): Option[Term] = {
    rules.view.flatMap { rule => trySimplificationOnTermWithRule(term, constraint, rule) }.headOption
  }

  def trySimplificationOnTermWithRule(term: Term, constraint: Constraint, rule: Rule): Option[Term] = {
    term.findSubTerms(t => t.instanceOf(rule.left)).filter((_, _, s) => Z3.constraintImplication(constraint.term, rule.constraint.term.applySubstitution(s))) match {
      case List() => None
      case (_, position, substitution)::_ => Some(term.rewriteAtPos(position, rule.right, substitution))
    }
  }

}
