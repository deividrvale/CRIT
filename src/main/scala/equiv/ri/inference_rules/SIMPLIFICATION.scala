package equiv.ri.inference_rules

import equiv.ri.{Equation, ProofState}
import equiv.ri.Equation.Side
import equiv.ri.inference_rules.SIMPLIFICATION
import equiv.trs.{Constraint, Rule, Term}
import equiv.trs.Term.{Position, Substitution, Var}
import equiv.utils.Z3
import equiv.utils.ListExtension.onNonEmpty

import scala.annotation.tailrec

object SIMPLIFICATION extends INFERENCE_RULE {
  val name = "SIMPLIFICATION"

  /** Try to apply SIMPLIFICATION on the given proofstate for the first equation, side, rule and subterm we find.
   * @param pfSt The [[ProofState]] to apply SIMPLIFICATION on.
   * @return [[Some]](proofstate) after application of SIMPLIFICATION, or [[None]] if no SIMPLIFICATION was possible. */
  @deprecated
  def trySimplification(pfSt: ProofState): Option[ProofState] = {
    pfSt.equations.view.flatMap( oldEquation => trySimplificationOnEquation(oldEquation, pfSt) ).headOption
  }

  /** Try to apply SIMPLIFICATION on the given equation for the first  side, rule and subterm we find.
   * @param equation The [[Equation]] to apply SIMPLIFICATION on.
   * @param pfSt The [[ProofState]] to apply SIMPLIFICATION on.
   * @return [[Some]](proofstate) after application of SIMPLIFICATION, or [[None]] if no SIMPLIFICATION was possible. */
  @deprecated
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
  @deprecated
  def trySimplificationOnEquationSide(equation: Equation, side: Side, pfSt: ProofState): Option[ProofState] = {
    (pfSt.hypotheses ++ pfSt.rules).view.flatMap( rule => trySimplificationOnEquationSideWithRule(equation, side, rule, pfSt) ).headOption
  }

  /** Try to apply SIMPLIFICATION on the given equation side with the given rule for the first subterm we find.
   * @param equation The [[Equation]] to apply SIMPLIFICATION on.
   * @param side The [[Side]] of the equation to apply SIMPLIFICATION on.
   * @param rule The [[Rule]] to use for SIMPLIFICATION.
   * @param pfSt The [[ProofState]] to apply SIMPLIFICATION on.
   * @return [[Some]](proofstate) after application of SIMPLIFICATION, or [[None]] if no SIMPLIFICATION was possible. */
  @deprecated
  def trySimplificationOnEquationSideWithRule(equation: Equation, side: Side, rule: Rule, pfSt: ProofState): Option[ProofState] = {
    val term = equation.getSide(side)
    getFirstPossibleRewritePlaceData(term, equation, rule)
      .map((_, position, substitution) => {
        doSimplificationOnEquationSideWithRuleAtPosition(equation, side, rule, position, substitution, pfSt)
      })
  }

  /** Given an [[Equation]], [[Side]], [[Rule]], [[Position]] and [[Substitution]] with a [[ProofState]], apply simplification using these coordinates.
   * @return The [[ProofState]] after SIMPLIFICATION */
  @deprecated
  def doSimplificationOnEquationSideWithRuleAtPosition(equation: Equation, side: Side, rule: Rule, position: Position, substitution: Substitution, pfSt: ProofState): ProofState = {
    val newEquation = equation.replaceSide(side, equation.getSide(side).rewriteAtPos(position, rule, substitution)).addConstraints(rule.constraints.map(_.applySubstitution(substitution)))
    println(s"${SIMPLIFICATION.name} on $side side of ${equation.toPrintString()} gives ${newEquation.toPrintString()}.")
    pfSt.replaceEquationWith(equation, newEquation)
  }

  /** Given a [[Term]] that is the left or right side of an equation, an [[Equation]] and a [[Rule]], get the first possible redex.
   * @return The first possible rewrite place: the subterm, position and substitution for the rule */
  @deprecated
  def getFirstPossibleRewritePlaceData(term: Term, equation: Equation, rule: Rule): Option[(Term, Position, Substitution)] = {
    getAllPossibleRewritePlacesData(term, equation, rule).headOption
  }

  /** Given a [[Term]] that is the left or right side of an equation, an [[Equation]] and a [[Rule]], get all possible redexes in the term.
   * @return All possible places where we can rewrite the given term with the given rule. */
  @deprecated
  def getAllPossibleRewritePlacesData(term: Term, equation: Equation, rule: Rule): List[(Term, Position, Substitution)] = {
    term
      .findSubTerms(_.instanceOf(rule.left))
      .filter((_, _, s: Substitution) =>
        val constraints = equation.getConstrainsConjunctAsTerm ;
        // Check if substitution s respects constraint ϕ, i.e. s(x) is a value for all x ∈ Var (ϕ) and [[ϕγ]] = T.
        s.forall { (x, sx) => !constraints.vars.contains(x) || sx.isValue } &&
        Z3.implies(
          constraints,
          rule.getConstrainsConjunctAsTerm.applySubstitution(s))
      )
  }

  /** Try to apply SIMPLIFICATION on the given [[ProofState]].
   * @param pfSt The current [[ProofState]] subject to SIMPLIFICATION.
   * @param equationSelector Function that selects an [[Equation]] from a non-empty list of [[Equation]]s.
   * @param sideSelector Function that selects a [[Side]] from a non-empty list of [[Side]]s.
   * @param ruleSelector Function that selects a [[Rule]] from a non-empty list of [[Rule]]s.
   * @param positionSelector Function that selects a [[Position]] from a non-empty list of [[Position]]s.
   * @return [[Some]]([[pfSt]]) after application of SIMPLIFICATION if possible, otherwise [[None]] */
  def trySIMPLIFICATION(pfSt: ProofState, equationSelector: List[Equation] => Equation, sideSelector: List[Side] => Side, ruleSelector: List[Rule] => Rule, positionSelector: List[Position] => Position): Option[ProofState] = {
    getSIMPLIFICATIONEquations(pfSt).onNonEmpty(
      eqs => trySIMPLIFICATIONOnEquation(pfSt, equationSelector(eqs), sideSelector, ruleSelector, positionSelector)
    )
  }

  /** Try to apply SIMPLIFICATION on the given [[ProofState]] and [[Equation]].
   * @param pfSt The current [[ProofState]] subject to SIMPLIFICATION.
   * @param equation The [[Equation]] subject to SIMPLIFICATION.
   * @param sideSelector Function that selects a [[Side]] from a non-empty list of [[Side]]s.
   * @param ruleSelector Function that selects a [[Rule]] from a non-empty list of [[Rule]]s.
   * @param positionSelector Function that selects a [[Position]] from a non-empty list of [[Position]]s.
   * @return [[Some]]([[pfSt]]) after application of SIMPLIFICATION if possible, otherwise [[None]] */
  def trySIMPLIFICATIONOnEquation(pfSt: ProofState, equation: Equation, sideSelector: List[Side] => Side, ruleSelector: List[Rule] => Rule, positionSelector: List[Position] => Position): Option[ProofState] = {
    getSIMPLIFICATIONEquationSides(pfSt, equation).onNonEmpty(
      sides => trySIMPLIFICATIONOnEquationSide(pfSt, equation, sideSelector(sides), ruleSelector, positionSelector)
    )
  }

  /** Try to apply SIMPLIFICATION on the given [[ProofState]] and [[Equation]] and [[Side]].
   * @param pfSt The current [[ProofState]] subject to SIMPLIFICATION.
   * @param equation The [[Equation]] subject to SIMPLIFICATION.
   * @param side The [[Side]] subject to SIMPLIFICATION.
   * @param ruleSelector Function that selects a [[Rule]] from a non-empty list of [[Rule]]s.
   * @param positionSelector Function that selects a [[Position]] from a non-empty list of [[Position]]s.
   * @return [[Some]]([[pfSt]]) after application of SIMPLIFICATION if possible, otherwise [[None]] */
  def trySIMPLIFICATIONOnEquationSide(pfSt: ProofState, equation: Equation, side: Side, ruleSelector: List[Rule] => Rule, positionSelector: List[Position] => Position): Option[ProofState] = {
    (getSIMPLIFICATIONEquationSideRules(pfSt, equation, side) ++ getSIMPLIFICATIONEquationSideHypotheses(pfSt, equation, side)).onNonEmpty(
      rules => trySIMPLIFICATIONOnEquationSideRule(pfSt, equation, side, ruleSelector(rules), positionSelector)
    )
  }

  /** Try to apply SIMPLIFICATION on the given [[ProofState]] and [[Equation]] and [[Side]] with the given [[Rule]].
   * @param pfSt The current [[ProofState]] subject to SIMPLIFICATION.
   * @param equation The [[Equation]] subject to SIMPLIFICATION.
   * @param side The [[Side]] subject to SIMPLIFICATION.
   * @param rule The [[Rule]] used for SIMPLIFICATION.
   * @param positionSelector Function that selects a [[Position]] from a non-empty list of [[Position]]s.
   * @return [[Some]]([[pfSt]]) after application of SIMPLIFICATION if possible, otherwise [[None]] */
  def trySIMPLIFICATIONOnEquationSideRule(pfSt: ProofState, equation: Equation, side: Side, rule: Rule, positionSelector: List[Position] => Position): Option[ProofState] = {
    getSIMPLIFICATIONEquationSideRuleRedexPositions(pfSt, equation, side, rule).onNonEmpty(
      positions => doSIMPLIFICATIONOnEquationSideRulePosition(pfSt, equation, side, rule, positionSelector(positions))
    )
  }

  /** Apply SIMPLIFICATION on the given [[ProofState]] and [[Equation]] and [[Side]] at the given [[Position]] with the given [[Rule]].
   * @param pfSt The current [[ProofState]] subject to SIMPLIFICATION.
   * @param equation The [[Equation]] subject to SIMPLIFICATION.
   * @param side The [[Side]] subject to SIMPLIFICATION.
   * @param rule The [[Rule]] used for SIMPLIFICATION.
   * @param position The [[Position]] where SIMPLIFICATION is applied. */
  def doSIMPLIFICATIONOnEquationSideRulePosition(pfSt: ProofState, equation: Equation, side: Side, rule: Rule, position: Position): Option[ProofState] = {
    equation.getSide(side).subTermAt(position).instanceOf(rule.left).map(substitution =>
      pfSt.removeEquation(equation).addEquation(equation.rewriteSideAtPos(side, position, rule, substitution)))
  }

  /** Helper variable that stores a [[List]] of [[Position]]s for some [[Equation]], [[Side]], [[Rule]]. */
  private var subtermPositions: Map[Equation, Map[Side, Map[Rule, List[Position]]]] = Map()

  /** @return A [[List]] of [[Equation]]s to which SIMPLIFICATION can be applied. May be empty. */
  def getSIMPLIFICATIONEquations(pfSt: ProofState): List[Equation] = {
    pfSt.equations.filter( eq => getSIMPLIFICATIONEquationSides(pfSt, eq).nonEmpty ).toList
  }

  /** @return A [[List]] of [[Side]]s for the given [[Equation]] to which SIMPLIFICATION can be applied. May be empty. */
  def getSIMPLIFICATIONEquationSides(pfSt: ProofState, equation: Equation): List[Side] = {
    List(Side.Left, Side.Right).filter( side => getSIMPLIFICATIONEquationSideRules(pfSt, equation, side).nonEmpty || getSIMPLIFICATIONEquationSideHypotheses(pfSt, equation, side).nonEmpty )
  }

  /** @return A [[List]] of [[Rules]] from [[pfSt.rules]] for the given [[Equation]] [[Side]] that can be used for a SIMPLIFICATION application. May be empty. */
  def getSIMPLIFICATIONEquationSideRules(pfSt: ProofState, equation: Equation, side: Side): List[Rule] = {
    pfSt.rules.filter( rule => getSIMPLIFICATIONEquationSideRuleRedexPositions(pfSt, equation, side, rule).nonEmpty ).toList
  }

  /** @return A [[List]] of [[Rules]] from [[pfSt.hypotheses]] for the given [[Equation]] [[Side]] that can be used for a SIMPLIFICATION application. May be empty. */
  def getSIMPLIFICATIONEquationSideHypotheses(pfSt: ProofState, equation: Equation, side: Side): List[Rule] = {
    pfSt.hypotheses.filter(hypothesis => getSIMPLIFICATIONEquationSideRuleRedexPositions(pfSt, equation, side, hypothesis).nonEmpty).toList
  }

  /** @return A [[List]] of [[Position]]s for the given [[Equation]], [[Side]] and [[Rule]] where SIMPLIFICATION can be performed. May be empty. */
  def getSIMPLIFICATIONEquationSideRuleRedexPositions(pfSt: ProofState, equation: Equation, side: Side, rule: Rule): List[Position] = {
    if subtermPositions.contains(equation) then
      if subtermPositions(equation).contains(side) then
        if subtermPositions(equation)(side).contains(rule) then
          subtermPositions(equation)(side)(rule)
        else
          val positions = getSIMPLIFICATIONEquationSideRuleRedexPositionsAux(pfSt, equation, side, rule)
          val updatedSide = subtermPositions(equation)(side) + (rule -> positions)
          val updatedEquation = subtermPositions(equation) + (side -> updatedSide)
          subtermPositions += (equation -> updatedEquation)
      else
        val positions = getSIMPLIFICATIONEquationSideRuleRedexPositionsAux(pfSt, equation, side, rule)
        val updatedEquation = subtermPositions(equation) + (side -> Map(rule -> positions))
        subtermPositions += (equation -> updatedEquation)
    else
      val positions = getSIMPLIFICATIONEquationSideRuleRedexPositionsAux(pfSt, equation, side, rule)
      subtermPositions += (equation -> Map(side -> Map(rule -> positions)))
    subtermPositions(equation)(side)(rule)
  }

  /** An auxiliary function for the [[getSIMPLIFICATIONEquationSideRuleRedexPositions]] method that returns all [[Position]]s where SIMPLIFICATION can be performed with the given [[Equation]], [[Side]] and [[Rule]] */
  def getSIMPLIFICATIONEquationSideRuleRedexPositionsAux(pfSt: ProofState, equation: Equation, side: Side, rule: Rule): List[Position] = {
    equation.getSide(side)
      .findSubTerms(_.instanceOf(rule.left))
      .filter((_, _, s: Substitution) =>
        val constraints = equation.getConstrainsConjunctAsTerm ;
        // Check if substitution s respects constraint ϕ, i.e. s(x) is a value for all x ∈ Var (ϕ) and [[ϕγ]] = T.
        s.forall { (x, sx) => !constraints.vars.contains(x) || sx.isValue } &&
          Z3.implies(
            constraints,
            rule.getConstrainsConjunctAsTerm.applySubstitution(s))
      )
      .map(_._2)
  }
}