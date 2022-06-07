package equiv
import equiv.trs.Core.*
import equiv.trs.Temp.*
import equiv.trs.{Constraint, Rule, Term}

/** A triple. We want to test if the left term is equal to the right under the given constraint. */
case class Equation(left : Term, right : Term, cons : Constraint)

/** A tuple
 * @param equations Set of equations to be solved
 * @param rules Set of rules that can be used in the rewriting induction */
case class ProofState(equations: Set[Equation], rules: Set[Rule])

/** Checks if the set of equations in a ProofState is empty, indicating that we are done with rewriting induction. */
def ProofStateIsComplete(pfst : ProofState) = pfst.equations.isEmpty

val eq = Equation(termFx, termReturnZero, Constraint(boolTrue))
val pfst = ProofState(Set(eq), system.rules)
