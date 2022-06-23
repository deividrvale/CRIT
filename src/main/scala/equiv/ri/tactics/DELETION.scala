package equiv.ri.tactics

import equiv.ri.ProofState
import equiv.utils.Z3
import equiv.ri.Equation

object DELETION {
    /** @return The given proofstate, without the equations that can be deleted, 
    i.e. equations with equal left and right hand terms and equations with unsatisfiable constraints */
    def tryDeletion(pfSt: ProofState): Option[ProofState] = {
        // Some(pfSt.copy(equations = pfSt.equations.filter(!deletable(_))))
        pfSt.equations.view.flatMap( e => deletable(e) ).headOption.map( equation => pfSt.copy(equations = pfSt.equations - equation) )
    }

    def deletable(equation: Equation): Option[Equation] = {
        if equation.left == equation.right || !Z3.isSatisfiable(equation.getConstrainsConjunctAsTerm) then
            println(s"DELETION on ${equation.toPrintString()}.")
            Some(equation)
        else 
            None
    }
}
