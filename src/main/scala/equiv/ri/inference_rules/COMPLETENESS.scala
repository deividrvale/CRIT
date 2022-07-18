package equiv.ri.inference_rules

import equiv.ri.ProofState
import equiv.ri.Equation

object COMPLETENESS extends INFERENCE_RULE {
  val name = "COMPLETENESS"

  /** The set of equations at the moment before the completeness flag was lost. This value is potentially changed when calling ``ProofState.setFlag``[[ProofState.setFlag()]] */
  var lastCompleteProofStateEquations: Option[Set[Equation]] = None

  /** If there exists a derivation sequence `(E, H, COMPLETE) |-* (E', H', INCOMPLETE)`, where `E'` is a subseteq of `E`, then we may deduce `|- (E', H', COMPLETE)` */
  def tryCOMPLETENESS(pfSt: ProofState): Option[ProofState] = {
    lastCompleteProofStateEquations.map( eqs => if pfSt.equations.subsetOf(eqs) then { pfSt.setFlag(true) } else return None )
  }
}
