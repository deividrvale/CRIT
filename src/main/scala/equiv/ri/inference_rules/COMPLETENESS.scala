package equiv.ri.inference_rules

import equiv.InputHandler
import equiv.ri.{CALCULATION_SIMP, Equation, ProofState}

object COMPLETENESS extends INFERENCE_RULE {
  val name = "COMPLETENESS"

  /** The set of equations at the moment before the completeness flag was lost. This value is potentially changed when calling ``ProofState.setFlag``[[ProofState.setFlag()]] */
  var lastCompleteProofStateEquations: Option[Set[Equation]] = None

  /** If there exists a derivation sequence `(E, H, COMPLETE) |-* (E', H', INCOMPLETE)`, where `E'` is a subseteq of `E`, then we may deduce `|- (E', H', COMPLETE)` */
  def tryCOMPLETENESS(pfSt: ProofState): Option[ProofState] = {
    lastCompleteProofStateEquations.map( lastCompleteEquations =>
      val simplifiedCurrentEquations = CALCULATION_SIMP.simplifyEquations(pfSt.equations)
      if simplifiedCurrentEquations.subsetOf(lastCompleteEquations)
        then { pfSt.setFlag(true) }
        else { InputHandler.errorMessage = "The current equation set is not a subset of the equation set of the last proofstate with COMPLETE flag." ;
          return None } )
  }
}
