package equiv.ri.tactics

import equiv.ri.{Equation, ProofState}
import equiv.ri.Equation.Side
import equiv.ri.tactics.SIMPLIFICATION
import equiv.trs.{Constraint, Rule, Term}
import equiv.trs.Term.{Position, Var}
import equiv.utils.Z3

import scala.annotation.tailrec

object SIMPLIFICATION {
  /** For each equation, try the `trySimplificationOnEquation` method */
  def trySimplification(pfSt: ProofState, rules: Set[Rule]): Option[(Equation, Equation)] = {
    pfSt.equations.view.flatMap { oldEquation =>
      trySimplificationOnEquation(oldEquation, rules).map((oldEquation,_))
    }.headOption
  }

  /** For each side of the equation, try the `trySimplificationOnEquationSide` method */
  def trySimplificationOnEquation(equation: Equation, rules: Set[Rule]): Option[Equation] = {
    List(Side.Left, Side.Right).view.flatMap{ side =>
      trySimplificationOnEquationSide(equation, side, rules)
    }.headOption
  }

  /** For each rule, try the `trySimplificationOnEquationSideWithRule` method */
  def trySimplificationOnEquationSide(equation: Equation, side: Side, rules: Set[Rule]): Option[Equation] = {
    rules.view.flatMap { rule => trySimplificationOnEquationSideWithRule(equation, side, rule) }.headOption
  }

  /** For the given equation and side, try the `trySimplificationOnTermWithRule` method */
  def trySimplificationOnEquationSideWithRule(equation: Equation, side: Side, rule: Rule): Option[Equation] = {
    val term = equation.getSide(side)
    getFirstPossibleRewritePlaceData(term, equation.constraint, rule)
      .map((_, position, substitution) =>
        equation
          .replaceSide(side,term.rewriteAtPos(position, rule.right, substitution))
          .addConstraint(rule.constraint.term.applySubstitution(substitution)))
  }

  /** @return The first possible rewrite place: the subterm, position and substitution for the rule */
  def getFirstPossibleRewritePlaceData(term: Term, constraint: Constraint, rule: Rule): Option[(Term, Position, Map[Var, Term])] = {
    term
      .findSubTerms(_.instanceOf(rule.left))
      .filter((_, _, s) => Z3.constraintImplication(constraint.term, rule.constraint.term.applySubstitution(s)))
      .headOption
  }

}
