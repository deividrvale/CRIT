package equiv.ri.inference_rules

import equiv.InputHandler
import equiv.ri.{Equation, ProofState}

object GENERALIZATION extends INFERENCE_RULE {
  val name = "GENERALIZATION"

  def doGENERALIZATION(pfSt: ProofState, oldEquation: Equation, newEquation: Equation): ProofState = {
    POSTULATE.doPOSTULATE(pfSt.removeEquation(oldEquation), Set(newEquation))
  }

  def tryGENERALIZATION(pfSt: ProofState, equationSelector: List[Equation] => Equation, newEquation: Equation): Option[ProofState] = {
    val oldEquation = equationSelector(pfSt.equations.toList)
    if oldEquation.instanceOf(newEquation).nonEmpty then
        Some(doGENERALIZATION(pfSt, oldEquation, newEquation))
      else {
        InputHandler.errorMessage = "New equation is not an instance of the selected equation." ; None
      }
  }
}
