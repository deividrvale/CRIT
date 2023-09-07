package equiv.ri.inference_rules

import equiv.ri.{ProofState, Equation}

object POSTULATE extends INFERENCE_RULE {
  val name = "POSTULATE"

  def doPOSTULATE(pfSt: ProofState, equations: Set[Equation]): ProofState = {
    pfSt.setFlag(false).addEquations(equations)
  }
}
