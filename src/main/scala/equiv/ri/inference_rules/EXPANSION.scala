package equiv.ri.inference_rules

import equiv.ri.{Equation, ProofState}
import equiv.trs.Rule
import equiv.ri.Equation.Side
import equiv.trs.Term
import equiv.trs.Term.{App, Position, Var}
import equiv.trs.Constraint
import equiv.utils.OptionExtension.getOnNone


object EXPANSION extends INFERENCE_RULE {
  val name = "EXPANSION"

  /** Try to apply EXPANSION on the first possible equation, side and subterm we find.
   * @param pfSt The current [[ProofState]] to try to apply EXPANSION on.
   * @param addRule Function that decides whether a generated [[Rule]] to the hypotheses set.
   * @return [[Some]](proofstate) after application of EXPANSION, or [[None]] if no SIMPLIFICATION was possible. */
  def tryExpansion(pfSt: ProofState, addRule: Rule => Boolean): Option[ProofState] = {
    pfSt.equations.view.flatMap( eq => tryExpansionOnEquation(eq, pfSt, addRule) ).headOption
  }

  /** Try to apply EXPANSION on the first possible side (of the given equation) and subterm we find.
   * @param equation The [[Equation]] to try to apply expansion on.
   * @param pfSt The current [[ProofState]] to try to apply EXPANSION on.
   * @param addRule Function that decides whether a generated [[Rule]] to the hypotheses set.
   * @return [[Some]](proofstate) after application of EXPANSION, or [[None]] if no SIMPLIFICATION was possible. */
  def tryExpansionOnEquation(equation: Equation, pfSt: ProofState, addRule: Rule => Boolean): Option[ProofState] = {
    List(Side.Left, Side.Right).view.flatMap( side => tryExpansionOnEquationSide(List(), equation, side, pfSt, addRule) ).headOption
  }

  /** Try to apply EXPANSION on the first possible subterm of ```equation.getSide(side).subTermAt(position)}}}``` we find.
   * @param position The [[Position]] of the subterm to try EXPANSION on. Should initially be ```List()``` (the empty list).
   * @param equation The [[Equation]] to try to apply EXPANSION on.
   * @param pfSt The current [[ProofState]] to try to apply EXPANSION on.
   * @param addRule Function that decides whether a generated [[Rule]] to the hypotheses set.
   * @return [[Some]](proofstate) after application of EXPANSION, or [[None]] if no SIMPLIFICATION was possible. */
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

  // TODO
  def getExpandableEquations(pfSt: ProofState): Set[Equation] = ???

  // TODO
  def getExpandableEquationSides(equation: Equation, pfSt: ProofState): Set[Side] = ???

  // TODO
  def getExpandableSubterms(term: Term, pfSt: ProofState): Set[Position] = ???
}
