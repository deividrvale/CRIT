package equiv.ri.inference_rules

import equiv.ri.{ProofState, Equation}

object POSTULATE extends INFERENCE_RULE {
  val name = "POSTULATE"

  def doPostulate(pfSt: ProofState, equations: Set[Equation]): ProofState = {
    println(s"POSTULATE equations ${equations.map(_.toPrintString())}.")
    pfSt.addEquations(equations).setFlag(false)
  }
}

object GENERALIZATION extends INFERENCE_RULE {
  val name = "GENERALIZATION"

  def doGeneralization(pfSt: ProofState, oldEquation: Equation, newEquation: Equation): ProofState = {
    POSTULATE.doPostulate(pfSt.removeEquation(oldEquation), Set(newEquation))
  }
}
