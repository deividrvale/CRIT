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
//    case (Var(_, _), i::rest, _) => error
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

}
