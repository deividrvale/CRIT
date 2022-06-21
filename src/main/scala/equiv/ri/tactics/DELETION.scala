package equiv.ri.tactics

import equiv.ri.ProofState
import equiv.utils.Z3
import equiv.ri.Equation

object DELETION {
    /** @return The given proofstate, without the equations that can be deleted, 
    i.e. equations with equal left and right hand terms and equations with unsatisfiable constraints */
    def tryDeletion(pfSt: ProofState): ProofState = {
        pfSt.copy(equations = pfSt.equations.filter(!deletable(_)))
    }

    def deletable(equation: Equation): Boolean = {
        if equation.left == equation.right || Z3.isSatisfiable(equation.getConstrainsConjunctAsTerm) then
            println(s"Deleted equation:    ${equation.toPrintString()}")
            true
        else 
            false
    }
}
