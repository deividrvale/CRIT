package equiv.ri.inference_rules

import equiv.ri.{Equation, ProofState}

object GENERALIZATION extends INFERENCE_RULE {
  val name = "GENERALIZATION"

  def doGENERALIZATION(pfSt: ProofState, oldEquation: Equation, newEquation: Equation): ProofState = {
    POSTULATE.doPOSTULATE(pfSt.removeEquation(oldEquation), Set(newEquation))
  }

  def tryGENERALIZATION(pfSt: ProofState, equation: Equation, newEquation: Equation): Option[ProofState] = {
    if equation.instanceOf(newEquation).nonEmpty then
      Some(doGENERALIZATION(pfSt, equation, newEquation))
    else None
  }
}
