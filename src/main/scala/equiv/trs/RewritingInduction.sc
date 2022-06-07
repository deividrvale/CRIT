import equiv.trs.{Constraint, Term}
import equiv.trs.*

case class Equation(left : Term, right : Term, cons : Constraint)

case class ProofState(equations: List[Equation], rules: List[Rule])

def ProofStateIsComplete(pfst : ProofState) = pfst.equations.isEmpty

