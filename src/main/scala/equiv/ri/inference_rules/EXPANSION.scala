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

  /** Try to apply EXPANSION on the given [[ProofState]].
   * @param pfSt The [[ProofState]] subject to EXPANSION.
   * @param equationSelector Function that selects an [[Equation]] from a non-empty list of [[Equation]]s.
   * @param sideSelector Function that selects a [[Side]] from a non-empty list of [[Side]]s.
   * @param subtermSelector Function that selects a subterm [[Position]] from a non-empty list of [[Position]]s.
   * @param ruleAcceptor Function that decides whether to add a [[Rule]] to the [[ProofState]].
   * @return [[Some]]([[pfSt]]) after application of EXPANSION if possible, otherwise [[None]] */
  def tryEXPANSION(pfSt: ProofState, equationSelector: List[Equation] => Equation, sideSelector: List[Side] => Side, subtermSelector: (Iterable[Term], List[Position]) => Position, ruleAcceptor: Rule => Boolean): Option[ProofState] = {
    getEXPANSIONEquations(pfSt).onNonEmpty(
      eqs => tryEXPANSIONOnEquation(pfSt, equationSelector(eqs), sideSelector, subtermSelector, ruleAcceptor)
    )
  }

  /** Try to apply EXPANSION on the given [[ProofState]] and [[Equation]].
   * @param pfSt The [[ProofState]] subject to EXPANSION.
   * @param equation The [[Equation]] subject to EXPANSION.
   * @param sideSelector Function that selects a [[Side]] from a non-empty list of [[Side]]s.
   * @param subtermSelector Function that selects a subterm [[Position]] from a non-empty list of [[Position]]s.
   * @param ruleAcceptor Function that decides whether to add a [[Rule]] to the [[ProofState]].
   * @return [[Some]]([[pfSt]]) after application of EXPANSION if possible, otherwise [[None]] */
  def tryEXPANSIONOnEquation(pfSt: ProofState, equation: Equation, sideSelector: List[Side] => Side, subtermSelector: (Iterable[Term], List[Position]) => Position, ruleAcceptor: Rule => Boolean): Option[ProofState] = {
    getEXPANSIONEquationSides(pfSt, equation).onNonEmpty(
      sides => tryEXPANSIONOnEquationSide(pfSt, equation, sideSelector(sides), subtermSelector, ruleAcceptor)
    )
  }

  /** Try to apply EXPANSION on the given [[ProofState]], [[Equation]] and [[Side]].
   * @param pfSt The [[ProofState]] subject to EXPANSION.
   * @param equation The [[Equation]] subject to EXPANSION.
   * @param side The [[Side]] of the [[Equation]] subject to EXPANSION.
   * @param subtermSelector Function that selects a subterm [[Position]] from a non-empty list of [[Position]]s.
   * @param ruleAcceptor Function that decides whether to add a [[Rule]] to the [[ProofState]].
   * @return [[Some]]([[pfSt]]) after application of EXPANSION if possible, otherwise [[None]] */
  def tryEXPANSIONOnEquationSide(pfSt: ProofState, equation: Equation, side: Side, subtermSelector: (Iterable[Term], List[Position]) => Position, ruleAcceptor: Rule => Boolean): Option[ProofState] = {
    getEXPANSIONEquationSideSubtermPositions(pfSt, equation, side).onNonEmpty(
      positions =>
        val newEquations = generateEXPANSIONEquations(pfSt, equation, side, subtermSelector(List(equation.getSide(side)), positions))
        val newPfSt = pfSt.removeEquation(equation).addEquations(newEquations)
        val rule = equation.getAsRule(side, true)
        if rule.left match { case App(fun, _) => fun.isTheory ; case _ => true } then
          Some(newPfSt)
        else if ruleAcceptor(rule) then
          Some(newPfSt.addHypothesis(rule))
        else Some(newPfSt)
    )
  }

  /** Apply EXPANSION to the given [[Position]] in the given [[Side]] of the [[Equation]] in the [[ProofState]]. */
  def generateEXPANSIONEquations(pfSt: ProofState, equation: Equation, side: Side, position: Position): Set[Equation] = {
    val constrainedTerm = equation.toConstrainedTerm
    val updatedPos = (if side == Side.Left then 0 else 1) :: position
    val applicableRules = pfSt.rules.flatMap(rule =>
      val renamedRule = rule.renameVarOccurrences(constrainedTerm.vars)
      renamedRule.left.unifiableWith(constrainedTerm.term.subTermAt(updatedPos)).map(sub => (renamedRule, sub)))
    applicableRules.map((rule, sub) => doEXPANSIONRule(constrainedTerm, updatedPos, rule, sub))
  }

  def doEXPANSIONRule(constrainedTerm: ConstrainedTerm, position: Position, rule: Rule, substitution: Substitution): Equation = {
    constrainedTerm.applySubstitution(substitution).rewriteAtPos(position, rule, substitution) match {
      case ConstrainedTerm(App(_, List(left, right)), cons) => Equation(left, right, cons).addConstraints(rule.substituteConstraints(substitution))
    }
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
  private var expansionSubterms: Map[Equation, Map[Side, List[Position]]] = Map()

  /** @return A [[List]] of [[Position]]s of every subterm in the given [[Side]] of the [[Equation]] in the [[ProofState]] subject to EXPANSION. */
  def getEXPANSIONEquationSideSubtermPositions(pfSt: ProofState, equation: Equation, side: Side): List[Position] = {
    if expansionSubterms.contains(equation) then
      if expansionSubterms(equation).contains(side) then
        return expansionSubterms(equation)(side)
      else
        val positions = getEXPANSIONEquationSideSubtermPositionsAux(pfSt, equation, side, List())
        val updatedEquationMap = expansionSubterms(equation) + (side -> positions)
        expansionSubterms += (equation -> updatedEquationMap)
    else
      val positions = getEXPANSIONEquationSideSubtermPositionsAux(pfSt, equation, side, List())
      expansionSubterms += (equation -> Map(side -> positions))
    expansionSubterms(equation)(side)
  }

  /** Helper function that returns a [[List]] of [[Position]]s where EXPANSION can be performed. */
  def getEXPANSIONEquationSideSubtermPositionsAux(pfSt: ProofState, equation: Equation, side: Side, position: Position): List[Position] = {
    val subterm = equation.getSide(side).subTermAt(position)
    if subterm.isBasic(pfSt.definedSymbols) then
      // Check if there is at least one applicable rule
      if pfSt.rules.view.map( rule => subterm.unifiableWith(rule.left) ).nonEmpty then List(position) else List()
      // Don't recurse, because subterms of a basic (sub)term are never basic
    else subterm match {
      case App(_, args) => args.indices.flatMap( id => getEXPANSIONEquationSideSubtermPositionsAux(pfSt, equation, side, position :+ id) ).toList
      case Var(_, _) => List()
    }
  }

}


















