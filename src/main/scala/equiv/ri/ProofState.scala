package equiv.ri

import equiv.ri.Equation.{Equation}
import equiv.ri.Equation.Side.Side
import equiv.trs.{Constraint, Rule, Term}
import equiv.trs.Term.Position

object ProofState {
  case class ProofState(equations: Set[Equation], rules: Set[Rule], flag: Boolean) {

    def SIMPLIFICATION(equation: Equation, side: Side, rule: Rule, position: Position): ProofState = {
      ProofState(equations - equation + equation.applyAtSide(side, _.substituteAtPos(position, rule.right)), rules, flag)
    }

    def EXPANSION(side: Side, equation: Equation): ProofState = {
      ProofState(equations ++ equation.getExpd(side), rules ++ equation.maybeHypothesis(side, rules), flag)
    }

    def DELETION(equation: Equation): ProofState = ProofState(equations - equation, rules, flag)

    def POSTULATE(equations2: Set[Equation]): ProofState = ProofState(equations ++ equations2, rules, false)

    def GENERALIZE(equation: Equation, equation2: Equation): ProofState = ProofState(equations - equation + equation2, rules, false)

    // TODO
    def EQDELETION(): ProofState = ???

    // TODO
    def CONSTRUCTOR(equation: Equation): ProofState = ProofState(equations - equation ++ equation.getConstructorArguments, rules, flag)

    // TODO
    def DISPROVE(): Option[Boolean] = if this.flag then Some(false) else None

    def COMPLETENESS(): ProofState = if !this.flag then ProofState(equations, rules, true) else this

    override def toString: String =
      s"( { ${equations.foldRight("")((e1, e2) => e1.toString ++ ", " ++ e2) } }, { ${rules.foldRight("")((r1, r2) => r1.toString ++ ", " ++ r2)} }, $flag )"
  }

}
