package equiv.ri
import equiv.trs.Core.boolTrue
import equiv.trs.Term.{App, Var}
import equiv.trs.{ConstrainedTerm, Constraint, Core, Rule, Term}

import scala.language.postfixOps

object Rewrite {
  def rewriteAtPos(constrainedTerm: ConstrainedTerm, position: List[Int], rule: Rule): ConstrainedTerm =
    ConstrainedTerm(
      constrainedTerm.term.substituteAtPos(position, rule.right),
      Constraint(boolTrue))

  def isConstraintSatisfied(constrainedTerm: ConstrainedTerm, rule: Rule): Boolean = true // TODO

  def findRedex(constrainedTerm: ConstrainedTerm, rule: Rule): List[Int] = ???

  /**  */
  def doTermsMatch(term1: Term, term2: Term) : Boolean = term1 match {
    case Var(_, _) => term1.instanceOf(term2).nonEmpty
    case App(_, args) => term1.instanceOf(term2).nonEmpty || args.map(doTermsMatch(_, term2)).foldLeft(false)(_ || _)
  }

  /** TODO Get a list of possible positions where we can apply the given rewrite rule in the term */
  def getPossibleRewritePositions(constrainedTerm: ConstrainedTerm, rule: Rule) : List[List[Int]] = ???

}

