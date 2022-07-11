package equiv.ri.inference_rules

import equiv.ri.ProofState
import equiv.utils.Z3
import equiv.ri.Equation

object DELETION {
    val name = "DELETION"

    /** Step-wise try to delete equations in the given proofstate.
     * After the first possible deletable equation, return the proofstate with this equation removed.
     * @return [[Some]](proofstate), where the proofstate has one equation less, or [[None]] if no equation could be deleted. */
    def tryDeletion(pfSt: ProofState): Option[ProofState] = {
      pfSt.equations.view.flatMap(tryDeletionOnEquation(_, pfSt)).headOption
    }

    /** Check if the given equation `s ~~ t [phi]` can be deleted, i.e. either `s = t` or `phi` is unsatisfiable.
     * If so, delete the equation from the given proofstate.
     * @param equation The equation that is checked for application.
     * @param pfSt The proofstate containing the equation.
     * @param succeedDebug Whether to print on a successful application.
     * @param failDebug Whether to print on a failed application.
     * @return [[Some]](proofstate), where the proofstate has one equation less, or [[None]] if no equation could be deleted. */
    def tryDeletionOnEquation(equation: Equation, pfSt: ProofState, succeedDebug: Boolean = true, failDebug: Boolean = false): Option[ProofState] = {
      if equation.left == equation.right || !Z3.satisfiable(equation.getConstrainsConjunctAsTerm) then
        if (succeedDebug) { println(s"$name on ${equation.toPrintString()}.") }
        Some(pfSt.removeEquation(equation))
      else
        if (failDebug) { println(s"$name failed.") }
        None
    }
}
