package equiv.ri.tactics

import equiv.ri.{Equation, ProofState}
import equiv.ri.Equation.Side
import equiv.ri.tactics.SIMPLIFICATION
import equiv.trs.{Constraint, Rule, Term}
import equiv.trs.Term.{Position, Substitution, Var}
import equiv.utils.{TermUtils, Z3}

import scala.annotation.tailrec

object SIMPLIFICATION {
  /** For each equation, try the `trySimplificationOnEquation` method */
  def trySimplification(pfSt: ProofState): Option[ProofState] = {
    pfSt.equations
      .view
      .flatMap( oldEquation => trySimplificationOnEquation(oldEquation, pfSt.rules).map( pfSt.replaceEquationWith(oldEquation,_) ) )
      .headOption
  }

  /** For each side of the equation, try the `trySimplificationOnEquationSide` method */
  def trySimplificationOnEquation(equation: Equation, rules: Set[Rule]): Option[Equation] = {
    List(Side.Left, Side.Right).view.flatMap{ side =>
      trySimplificationOnEquationSide(equation, side, rules)
    }.headOption
  }

  /** For each rule, try the `trySimplificationOnEquationSideWithRule` method */
  def trySimplificationOnEquationSide(equation: Equation, side: Side, rules: Set[Rule]): Option[Equation] = {
    rules.view.flatMap( rule => trySimplificationOnEquationSideWithRule(equation, side, rule) ).headOption
  }

  /** For the given equation and side, try the `trySimplificationOnTermWithRule` method */
  def trySimplificationOnEquationSideWithRule(equation: Equation, side: Side, rule: Rule): Option[Equation] = {
    val term = equation.getSide(side)
    getFirstPossibleRewritePlaceData(term, equation, rule)
      .map((_, position, substitution) => {
        doSimplificationOnEquationSideWithRuleAtPosition(equation, side, rule, position, substitution)
      })
  }

  def doSimplificationOnEquationSideWithRuleAtPosition(equation: Equation, side: Side, rule: Rule, position: Position, substitution: Substitution): Equation = {
    val newEquation = equation.replaceSide(side, equation.getSide(side).rewriteAtPos(position, rule, substitution)).addConstraints(rule.constraints.map(_.applySubstitution(substitution)))
    println(s"SIMPLIFICATION on $side side of ${equation.toPrintString()} gives ${newEquation.toPrintString()}.")
    newEquation
  }

  /** @return The first possible rewrite place: the subterm, position and substitution for the rule */
  def getFirstPossibleRewritePlaceData(term: Term, equation: Equation, rule: Rule): Option[(Term, Position, Map[Var, Term])] = {
    getAllPossibleRewritePlacesData(term, equation, rule).headOption
  }

  /** @return All possible places where we can rewrite $term with $rule. */
  def getAllPossibleRewritePlacesData(term: Term, equation: Equation, rule: Rule): List[(Term, Position, Map[Var, Term])] = {
    term
      .findSubTerms(_.instanceOf(rule.left))
      .filter((_, _, s) => Z3.implies(
        equation.getConstrainsConjunctAsTerm,
        rule.getConstrainsConjunctAsTerm.applySubstitution(s))
      )
  }

}
