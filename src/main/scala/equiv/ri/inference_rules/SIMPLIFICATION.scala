package equiv.ri.inference_rules

import equiv.ri.{Equation, ProofState}
import equiv.ri.Equation.Side
import equiv.ri.inference_rules.SIMPLIFICATION
import equiv.trs.{Constraint, Rule, Term}
import equiv.trs.Term.{Position, Substitution, Var}
import equiv.utils.{TermUtils, Z3}

import scala.annotation.tailrec

object SIMPLIFICATION {
  val name = "SIMPLIFICATION"

  /** Try to apply SIMPLIFICATION on the given proofstate for the first equation, side, rule and subterm we find.
   * @param pfSt The [[ProofState]] to apply SIMPLIFICATION on.
   * @return [[Some]](proofstate) after application of SIMPLIFICATION, or [[None]] if no SIMPLIFICATION was possible. */
  def trySimplification(pfSt: ProofState): Option[ProofState] = {
    pfSt.equations.view.flatMap( oldEquation => trySimplificationOnEquation(oldEquation, pfSt) ).headOption
  }

  /** Try to apply SIMPLIFICATION on the given equation for the first  side, rule and subterm we find.
   * @param equation The [[Equation]] to apply SIMPLIFICATION on.
   * @param pfSt The [[ProofState]] to apply SIMPLIFICATION on.
   * @return [[Some]](proofstate) after application of SIMPLIFICATION, or [[None]] if no SIMPLIFICATION was possible. */
  def trySimplificationOnEquation(equation: Equation, pfSt: ProofState): Option[ProofState] = {
    List(Side.Left, Side.Right).view.flatMap{ side =>
      trySimplificationOnEquationSide(equation, side, pfSt)
    }.headOption
  }

  /** Try to apply SIMPLIFICATION on the given equation side for the first rule and subterm we find.
   * @param equation The [[Equation]] to apply SIMPLIFICATION on.
   * @param side The [[Side]] of the equation to apply SIMPLIFICATION on.
   * @param pfSt The [[ProofState]] to apply SIMPLIFICATION on.
   * @return [[Some]](proofstate) after application of SIMPLIFICATION, or [[None]] if no SIMPLIFICATION was possible. */
  def trySimplificationOnEquationSide(equation: Equation, side: Side, pfSt: ProofState): Option[ProofState] = {
    (pfSt.hypotheses ++ pfSt.rules).view.flatMap( rule => trySimplificationOnEquationSideWithRule(equation, side, rule, pfSt) ).headOption
  }

  /** Try to apply SIMPLIFICATION on the given equation side with the given rule for the first subterm we find.
   * @param equation The [[Equation]] to apply SIMPLIFICATION on.
   * @param side The [[Side]] of the equation to apply SIMPLIFICATION on.
   * @param rule The [[Rule]] to use for SIMPLIFICATION.
   * @param pfSt The [[ProofState]] to apply SIMPLIFICATION on.
   * @return [[Some]](proofstate) after application of SIMPLIFICATION, or [[None]] if no SIMPLIFICATION was possible. */
  def trySimplificationOnEquationSideWithRule(equation: Equation, side: Side, rule: Rule, pfSt: ProofState): Option[ProofState] = {
    val term = equation.getSide(side)
    getFirstPossibleRewritePlaceData(term, equation, rule)
      .map((_, position, substitution) => {
        doSimplificationOnEquationSideWithRuleAtPosition(equation, side, rule, position, substitution, pfSt)
      })
  }

  /** Given an [[Equation]], [[Side]], [[Rule]], [[Position]] and [[Substitution]] with a [[ProofState]], apply simplification using these coordinates.
   * @return The [[ProofState]] after SIMPLIFICATION */
  def doSimplificationOnEquationSideWithRuleAtPosition(equation: Equation, side: Side, rule: Rule, position: Position, substitution: Substitution, pfSt: ProofState): ProofState = {
    val newEquation = equation.replaceSide(side, equation.getSide(side).rewriteAtPos(position, rule, substitution)).addConstraints(rule.constraints.map(_.applySubstitution(substitution)))
    println(s"${SIMPLIFICATION.name} on $side side of ${equation.toPrintString()} gives ${newEquation.toPrintString()}.")
    pfSt.replaceEquationWith(equation, newEquation)
  }

  /** Given a [[Term]] that is the left or right side of an equation, an [[Equation]] and a [[Rule]], get the first possible redex.
   * @return The first possible rewrite place: the subterm, position and substitution for the rule */
  def getFirstPossibleRewritePlaceData(term: Term, equation: Equation, rule: Rule): Option[(Term, Position, Map[Var, Term])] = {
    getAllPossibleRewritePlacesData(term, equation, rule).headOption
  }

  /** Given a [[Term]] that is the left or right side of an equation, an [[Equation]] and a [[Rule]], get all possible redexes in the term.
   * @return All possible places where we can rewrite the given term with the given rule. */
  def getAllPossibleRewritePlacesData(term: Term, equation: Equation, rule: Rule): List[(Term, Position, Map[Var, Term])] = {
    term
      .findSubTerms(_.instanceOf(rule.left))
      .filter((_, _, s) => Z3.implies(
        equation.getConstrainsConjunctAsTerm,
        rule.getConstrainsConjunctAsTerm.applySubstitution(s))
      )
  }

}
