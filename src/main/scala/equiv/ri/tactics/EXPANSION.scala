package equiv.ri.tactics

import equiv.ri.{Equation, ProofState}
import equiv.trs.Rule
import equiv.ri.Equation.Side


object EXPANSION {

  def tryExpansion(pfSt: ProofState): Option[(Equation, Set[Equation], Option[Rule])] = {
    pfSt.equations.view.flatMap { equation =>
      tryExpansionOnEquation(equation, pfSt.rules).map(
        (equations, maybeRule) => (equation, equations, maybeRule)
      )
    }.headOption
  }

  def tryExpansionOnEquation(equation: Equation, rules: Set[Rule]): Option[(Set[Equation], Option[Rule])] = {
    None
  }

  def tryExpansionOnEquationSide(equation: Equation, side: Side, rules: Set[Rule]): Option[(Set[Equation], Option[Rule])] = {
    None
  }

}
