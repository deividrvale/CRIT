package equiv.ri.tactics

import equiv.ri.ProofState
import equiv.utils.Z3
import equiv.ri.Equation

object DELETION {
    /** @return The given proofstate, without the equations that can be deleted, 
    i.e. equations with equal left and right hand terms and equations with unsatisfiable constraints,
    or None if there are no equations to be deleted */
    def tryDeletion(pfSt: ProofState): Option[ProofState] = {
        pfSt.equations.view.flatMap( e => deletable(e) ).headOption.map( equation => pfSt.copy(equations = pfSt.equations - equation) )
    }

    def deletable(equation: Equation): Option[Equation] = {
        if equation.left == equation.right || !Z3.satisfiable(equation.getConstrainsConjunctAsTerm) then
            println(s"DELETION on ${equation.toPrintString()}.")
            Some(equation)
        else 
            None
    }
}
