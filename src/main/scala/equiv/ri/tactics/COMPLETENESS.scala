package equiv.ri.tactics

import equiv.ri.ProofState
import equiv.ri.Equation

object COMPLETENESS {
  /** The set of equations at the moment before the completeness flag was lost. This value is changed in `ProofState.setFlag`. */
  var lastCompleteProofStateEquations: Option[Set[Equation]] = None

  /** If there exists a derivation sequence `(E, H, COMPLETE) |-* (E', H', INCOMPLETE)`, where `E'` is a subseteq of `E`, then we may deduce `|- (E', H', COMPLETE)` */
  def tryCompleteness(pfSt: ProofState): Option[ProofState] = {
    lastCompleteProofStateEquations.map( eqs => if pfSt.equations.subsetOf(eqs) then { println("COMPLETENESS on proofstate") ; pfSt.setFlag(true) } else return None )
  }
}
