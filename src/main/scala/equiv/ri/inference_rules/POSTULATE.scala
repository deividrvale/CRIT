package equiv.ri.inference_rules

import equiv.ri.{ProofState, Equation}

object POSTULATE extends INFERENCE_RULE {
  val name = "POSTULATE"

  def doPOSTULATE(pfSt: ProofState, equations: Set[Equation]): ProofState = {
    pfSt.addEquations(equations).setFlag(false)
  }
}

object GENERALIZATION extends INFERENCE_RULE {
  val name = "GENERALIZATION"

  def doGENERALIZATION(pfSt: ProofState, oldEquation: Equation, newEquation: Equation): ProofState = {
    POSTULATE.doPOSTULATE(pfSt.removeEquation(oldEquation), Set(newEquation))
  }
}
