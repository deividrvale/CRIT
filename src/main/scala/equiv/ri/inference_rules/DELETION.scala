package equiv.ri.inference_rules

import equiv.ri.ProofState
import equiv.utils.Z3
import equiv.ri.Equation
import equiv.utils.ListExtension.onNonEmpty

object DELETION extends INFERENCE_RULE {
    val name = "DELETION"

    /** Try to apply DELETION on the given [[ProofState]].
     * @param pfSt The current [[ProofState]].
     * @param equationSelector A function that selects an [[Equation]] from a non-empty list of [[Equation]]s.
     * @return The [[Some]]([[ProofState]]) after application of DELETION or [[None]] if DELETION was not possible. */
    def tryDELETION(pfSt: ProofState, equationSelector: List[Equation] => Equation): Option[ProofState] = {
      getDELETIONEquations(pfSt).onNonEmpty(
        eqs => Some( pfSt.removeEquation( equationSelector(eqs) ) )
      )
    }

    /** @return A [[List]] of all [[Equation]]s to which the DELETION rule can be applied. May be empty. */
    def getDELETIONEquations(pfSt: ProofState): List[Equation] = {
      pfSt.equations.filter(eq => eq.left == eq.right || !Z3.satisfiable(eq.getConstrainsConjunctAsTerm)).toList
    }
}
