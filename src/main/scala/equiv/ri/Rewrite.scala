package equiv.ri
import equiv.trs.Core.boolTrue
import equiv.trs.Term.{App, Var, Position}
import equiv.trs.{ConstrainedTerm, Constraint, Core, Rule, Term}

import scala.language.postfixOps

object Rewrite {
  def rewriteAtPos(constrainedTerm: ConstrainedTerm, position: Position, rule: Rule): ConstrainedTerm =
    ConstrainedTerm(
      constrainedTerm.term.substituteAtPos(position, rule.right),
      Constraint(boolTrue))

  /** TODO Get a list of possible positions where we can apply the given rewrite rule in the term */
  def getPossibleRewritePositions(constrainedTerm: ConstrainedTerm, rule: Rule) : List[Position] = ???

}

