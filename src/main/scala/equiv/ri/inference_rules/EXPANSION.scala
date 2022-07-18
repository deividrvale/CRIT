package equiv.ri.inference_rules

import equiv.ri.{Equation, ProofState}
import equiv.trs.{ConstrainedTerm, Constraint, Rule, Term}
import equiv.ri.Equation.Side
import equiv.trs.Term.{App, Position, Substitution, Var}
import equiv.utils.OptionExtension.getOnNone
import equiv.utils.ListExtension.onNonEmpty
import equiv.utils.TermUtils

import scala.annotation.tailrec


object EXPANSION extends INFERENCE_RULE {
  val name = "EXPANSION"

  /** Try to apply EXPANSION on the first possible equation, side and subterm we find.
   * @param pfSt The current [[ProofState]] to try to apply EXPANSION on.
   * @param addRule Function that decides whether a generated [[Rule]] to the hypotheses set.
   * @return [[Some]](proofstate) after application of EXPANSION, or [[None]] if no SIMPLIFICATION was possible. */
  @deprecated
  def tryExpansion(pfSt: ProofState, addRule: Rule => Boolean): Option[ProofState] = {
    pfSt.equations.view.flatMap( eq => tryExpansionOnEquation(eq, pfSt, addRule) ).headOption
  }

  /** Try to apply EXPANSION on the first possible side (of the given equation) and subterm we find.
   * @param equation The [[Equation]] to try to apply expansion on.
   * @param pfSt The current [[ProofState]] to try to apply EXPANSION on.
   * @param addRule Function that decides whether a generated [[Rule]] to the hypotheses set.
   * @return [[Some]](proofstate) after application of EXPANSION, or [[None]] if no SIMPLIFICATION was possible. */
  @deprecated
  def tryExpansionOnEquation(equation: Equation, pfSt: ProofState, addRule: Rule => Boolean): Option[ProofState] = {
    List(Side.Left, Side.Right).view.flatMap( side => tryExpansionOnEquationSide(List(), equation, side, pfSt, addRule) ).headOption
  }

  /** Try to apply EXPANSION on the first possible subterm of ```equation.getSide(side).subTermAt(position)}}}``` we find.
   * @param position The [[Position]] of the subterm to try EXPANSION on. Should initially be ```List()``` (the empty list).
   * @param equation The [[Equation]] to try to apply EXPANSION on.
   * @param pfSt The current [[ProofState]] to try to apply EXPANSION on.
   * @param addRule Function that decides whether a generated [[Rule]] to the hypotheses set.
   * @return [[Some]](proofstate) after application of EXPANSION, or [[None]] if no SIMPLIFICATION was possible. */
  @deprecated
  def tryExpansionOnEquationSide(position: Position = List(), equation: Equation, side: Side, pfSt: ProofState, addRule: Rule => Boolean): Option[ProofState] = {
    equation.getSide(side).subTermAt(position) match {
      case Var(_, _) => None
      case App(_, args) => tryExpansionOnEquationSideSubterm(position, equation, side, pfSt, addRule).getOnNone(
        args.indices.view.flatMap(id => tryExpansionOnEquationSide(position :+ id, equation, side, pfSt, addRule) ).headOption
      )
    }
  }

  /** Try to apply EXPANSION on the first possible subterm we find.
   * @param position The [[Position]] of the subterm to try EXPANSION on.
   * @param equation The [[Equation]] to try to apply EXPANSION on.
   * @param pfSt The current [[ProofState]] to try to apply EXPANSION on.
   * @param addRule Function that decides whether a generated [[Rule]] to the hypotheses set.
   * @return [[Some]](proofstate) after application of EXPANSION, or [[None]] if no SIMPLIFICATION was possible. */
  @deprecated
  def tryExpansionOnEquationSideSubterm(position: Position, equation: Equation, side: Side, pfSt: ProofState, addRule: Rule => Boolean): Option[ProofState] = {
    val subterm = equation.getSide(side).subTermAt(position)
    if subterm.isBasic(pfSt.definedSymbols) then {
      val applicableRuleSubstitutionPairs = pfSt.rules.flatMap( rule => subterm.instanceOf(rule.left).map((rule, _)) )
      if applicableRuleSubstitutionPairs.nonEmpty then {
        // Expd logic
        val rHSsAndConstraintsPairs = applicableRuleSubstitutionPairs.map( (rule, sub) => (subterm.rewriteAtPos(List(), rule, sub), rule.substituteConstraints(sub)) )
        val equations = rHSsAndConstraintsPairs.map((rhs, cons) => equation.replaceSide(side, rhs).addConstraints(cons))
        println(s"$name on $side side of ${equation.toPrintString()} gives ${equations.map(_.toPrintString())}.")
        var newPfSt = pfSt.removeEquation(equation).addEquations(equations)
        // Rule logic
        val rule = Rule(equation.getSide(side), equation.getOppositeSide(side), equation.constraints)
        if addRule(rule) then newPfSt = newPfSt.addHypothesis(rule)
        // Return
        return Some(newPfSt)
      } // else { println("No applicable rules.") }
    } // else { println("Not basic.") }
    None
  }

  /** Try to apply EXPANSION on the given [[ProofState]].
   * @param pfSt The [[ProofState]] subject to EXPANSION.
   * @param equationSelector Function that selects an [[Equation]] from a non-empty list of [[Equation]]s.
   * @param sideSelector Function that selects a [[Side]] from a non-empty list of [[Side]]s.
   * @param subtermSelector Function that selects a subterm [[Position]] from a non-empty list of [[Position]]s.
   * @param ruleSelector Function that decides whether to add a [[Rule]] to the [[ProofState]].
   * @return [[Some]]([[pfSt]]) after application of EXPANSION if possible, otherwise [[None]] */
  def tryEXPANSION(pfSt: ProofState, equationSelector: List[Equation] => Equation, sideSelector: List[Side] => Side, subtermSelector: List[Position] => Position, ruleSelector: Rule => Boolean): Option[ProofState] = {
    getEXPANSIONEquations(pfSt).onNonEmpty(
      eqs => tryEXPANSIONOnEquation(pfSt, equationSelector(eqs), sideSelector, subtermSelector, ruleSelector)
    )
  }

  /** Try to apply EXPANSION on the given [[ProofState]] and [[Equation]].
   * @param pfSt The [[ProofState]] subject to EXPANSION.
   * @param equation The [[Equation]] subject to EXPANSION.
   * @param sideSelector Function that selects a [[Side]] from a non-empty list of [[Side]]s.
   * @param subtermSelector Function that selects a subterm [[Position]] from a non-empty list of [[Position]]s.
   * @param ruleSelector Function that decides whether to add a [[Rule]] to the [[ProofState]].
   * @return [[Some]]([[pfSt]]) after application of EXPANSION if possible, otherwise [[None]] */
  def tryEXPANSIONOnEquation(pfSt: ProofState, equation: Equation, sideSelector: List[Side] => Side, subtermSelector: List[Position] => Position, ruleSelector: Rule => Boolean): Option[ProofState] = {
    getEXPANSIONEquationSides(pfSt, equation).onNonEmpty(
      sides => tryEXPANSIONOnEquationSide(pfSt, equation, sideSelector(sides), subtermSelector, ruleSelector)
    )
  }

  /** Try to apply EXPANSION on the given [[ProofState]], [[Equation]] and [[Side]].
   * @param pfSt The [[ProofState]] subject to EXPANSION.
   * @param equation The [[Equation]] subject to EXPANSION.
   * @param side The [[Side]] of the [[Equation]] subject to EXPANSION.
   * @param subtermSelector Function that selects a subterm [[Position]] from a non-empty list of [[Position]]s.
   * @param ruleSelector Function that decides whether to add a [[Rule]] to the [[ProofState]].
   * @return [[Some]]([[pfSt]]) after application of EXPANSION if possible, otherwise [[None]] */
  def tryEXPANSIONOnEquationSide(pfSt: ProofState, equation: Equation, side: Side, subtermSelector: List[Position] => Position, ruleSelector: Rule => Boolean): Option[ProofState] = {
    getEXPANSIONEquationSideSubtermPositions(pfSt, equation, side).onNonEmpty(
      positions =>
        val newEquations = doEXPANSION(pfSt, equation, side, subtermSelector(positions))
        var newPfSt = pfSt.removeEquation(equation).addEquations(newEquations)
        val rule = equation.getRule(side)
        if ruleSelector(rule) then newPfSt = newPfSt.addHypothesis(rule)
        Some(newPfSt)
    )
  }

  /** @return A [[List]] of [[Equation]]s from a [[ProofState]] to which EXPANSION can be applied. */
  def getEXPANSIONEquations(pfSt: ProofState): List[Equation] = {
    pfSt.equations.filter( eq => getEXPANSIONEquationSides(pfSt, eq).nonEmpty ).toList
  }

  /** A [[List]] of [[Side]]s of an [[Equation]] from a [[ProofState]] to which EXPANSION can be applied. */
  def getEXPANSIONEquationSides(pfSt: ProofState, equation: Equation): List[Side] = {
    List(Side.Left, Side.Right).filter( side => getEXPANSIONEquationSideSubtermPositions(pfSt, equation, side).nonEmpty )
  }

  /** Storage variable for every subterm position subject to EXPANSION, together with the side and equation. */
  private var expansionSubterms: Option[Map[Equation, Map[Side, List[Position]]]] = None

  /** @return A [[List]] of [[Position]]s of every subterm in the given [[Side]] of the [[Equation]] in the [[ProofState]] subject to EXPANSION. */
  def getEXPANSIONEquationSideSubtermPositions(pfSt: ProofState, equation: Equation, side: Side): List[Position] = {
    expansionSubterms match {
      case Some(subterms) => subterms(equation)(side)
      case None =>
        val subterms = getEXPANSIONEquationSideSubtermPositionsAux(pfSt, equation, side, List())
        expansionSubterms = Some(subterms)
        subterms(equation)(side)
    }
  }

  /** Helper function that constructs the [[expansionSubterms]] variable. */
  def getEXPANSIONEquationSideSubtermPositionsAux(pfSt: ProofState, equation: Equation, side: Side, position: Position): Map[Equation, Map[Side, List[Position]]] = {
    val subterm = equation.getSide(side).subTermAt(position)
    if subterm.isBasic(pfSt.definedSymbols) then
      // Check if there is at least one applicable rule
      if pfSt.rules.view.map( rule => subterm.instanceOf(rule.left) ).nonEmpty then Map((equation, Map((side, List(position))))) else Map()
      // Don't recurse, because subterms of a basic (sub)term are never basic
    else subterm match {
      case App(_, args) => args.indices.flatMap( id => getEXPANSIONEquationSideSubtermPositionsAux(pfSt, equation, side, position :+ id) ).toMap
      case Var(_, _) => Map()
    }
  }

  /** Apply EXPANSION to the given [[Position]] in the given [[Side]] of the [[Equation]] in the [[ProofState]]. */
  def doEXPANSION(pfSt: ProofState, equation: Equation, side: Side, position: Position): Set[Equation] = {
    val constrainedTerm = ConstrainedTerm(App(TermUtils.getEqualityFunctionSymbol(equation), List(equation.left, equation.right)), equation.constraints)
    val updatedPos = (if side == Side.Left then 0 else 1) :: position
    val applicableRules = pfSt.rules.flatMap( rule => constrainedTerm.term.subTermAt(updatedPos).instanceOf(rule.left).map( sub => (rule, sub) ) )
    applicableRules.map( (rule, sub) => constrainedTerm.rewriteAtPos(updatedPos, rule, sub) match { case ConstrainedTerm(App(_, List(left, right)), cons) => Equation(left, right, cons) } )
  }
}


















