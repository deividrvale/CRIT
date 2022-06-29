package equiv.ri.tactics

import equiv.ri.{ProofState, Equation}

object POSTULATE {
  def doPostulate(pfSt: ProofState, equations: Set[Equation]): ProofState = {
    println(s"POSTULATE equations ${equations.map(_.toPrintString())}.")
    pfSt.addEquations(equations).setFlag(false)
  }
}

object GENERALIZATION {
  def doGeneralization(pfSt: ProofState, oldEquation: Equation, newEquation: Equation): ProofState = {
    POSTULATE.doPostulate(pfSt.removeEquation(oldEquation), Set(newEquation))
  }
}
