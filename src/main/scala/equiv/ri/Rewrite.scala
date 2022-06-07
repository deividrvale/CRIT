package equiv.ri
import equiv.trs.Core.boolTrue
import equiv.trs.Term.{App, Var}
import equiv.trs.{ConstrainedTerm, Constraint, Core, Rule, Term}

object Rewrite {

  /** Substitute a sub-term of `term1` with `term2` at the given `position`.
   * @param term1 Term in which a sub-term is substituted
   * @param position Position of substitution as a list of Ints
   * @param term2 Term to substitute */
  def substituteAtPos(term1: Term, position: List[Int], term2: Term): Term = (term1, position, term2) match {
    case (Var(_, _), List(), _) => term2
//    case (Var(_, _), x::xs, _) => error
    case (App(_, _), List(), _) => term2
    case (App(f, args), x::xs, t2) => App(f, args.updated(x, substituteAtPos(args(x), xs, t2) ) )
  }

  /** Substitute all occurrences of `matchTerm` in `mainTerm` by `replacementTerm`
   * @param mainTerm Main term where sub-terms will be replaced
   * @param matchTerm Sub-term that will be replaced
   * @param replacementTerm Term that will replace occurrences of `matchTerm` */
  def substituteAll(mainTerm: Term, matchTerm: Term, replacementTerm: Term): Term = (mainTerm, matchTerm) match {
    case (x@Var(_, _), y) => if x == y then replacementTerm else x
    case (x@App(x1, x2), y) => if x == y then replacementTerm else App(x1, x2.map(t => substituteAll(t, y, replacementTerm)))
  }

  def rewriteAtPos(constrainedTerm: ConstrainedTerm, position: List[Int], rule: Rule): ConstrainedTerm =
    ConstrainedTerm(
      substituteAtPos(constrainedTerm.term, position, rule.right),
      Constraint(boolTrue))

  /** @return A list of variables that occur in the given term */
  def getTermVars(term: Term): List[Term] = term match {
    case x@Var(_, _) => List(x)
    case App(_, args) => args.flatMap(getTermVars)
  }

  /** @return A list of variables that occur in the given constrained term */
  def getConstrainedTermVars(ct: ConstrainedTerm): List[Term] = getTermVars(ct.term) ++ getTermVars(ct.constraint.term)

  /** Checks whether a certain rewrite rule is applicable somewhere in the given constrained term */
  def isRuleApplicable(constrainedTerm: ConstrainedTerm, rule: Rule) : Boolean = ???

  /** Checks whether a certain rewrite rule is applicable at the root of the given constrained term */
  def isRuleApplicableAtRoot(constrainedTerm: ConstrainedTerm, rule: Rule) : Boolean = matchTerms(constrainedTerm.term, rule.left)

  /**  */
  def matchTerms(term1: Term, term2: Term) : Boolean = (term1, term2) match {
    case (Var(_, sort1), Var(_, sort2)) => sort1 == sort2
    case (Var(_, _), App(_, _)) => false
    case (App(f, _), Var(_, sort)) => f.typing.output == sort
    case (App(f1, args1), App(f2, args2)) => f1 == f2 && args1.length == args2.length &&
      (args1 zip args2).map(matchTerms).foldLeft(true)(_ && _)
  }

  /** Get a list of possible positions where we can apply the given rewrite rule in the term */
  def getPossibleRewritePositions(constrainedTerm: ConstrainedTerm, rule: Rule) : List[List[Int]] = ???

}

