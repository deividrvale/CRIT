package equiv.trs

import equiv.trs.Term.*
import equiv.utils.{MapUtils, TermUtils}

case class ConstrainedTerm(term: Term, constraints: Set[Constraint]) extends ConstrainedObject(constraints) {
  /** @return A list of variables that occur in this constrained term */
  def vars: Set[Var] = term.vars ++ constraints.flatMap(_.term.vars)

  def instanceOf(other: ConstrainedTerm): Option[Substitution] = {
    // TODO

None
//    if (this.sort != other.sort) None else
//      (this, other) match {
//        case (_, v@Var(_, _)) => Some(Map(v -> this))
//        case (Var(_, _), App(_, _)) => None
//        case (App(f1, args1), App(f2, args2)) =>
//          if (f1 == f2) {
//            (args1 zip args2).map(_.instanceOf(_)).foldLeft[Option[Substitution]](Some(Map.empty)) {
//              case (Some(map1), Some(map2)) => MapUtils.union(map1, map2)
//              case _ => None
//            }
//          } else None
//      }
  }

  /** Substitute a subterm with `replacement` at the given `position`.
   * @param position Position of substitution as a list of Ints
   * @param replacement Term to substitute */
  def substituteAtPos(position: Position, replacement: Term): ConstrainedTerm =
    ConstrainedTerm(term.substituteAtPos(position, replacement), constraints)

  def rewriteAtPos(position: Position, rule: Rule, substitution: Substitution): ConstrainedTerm =
    ConstrainedTerm(term.rewriteAtPos(position, rule, substitution), constraints)

  def applySubstitution(substitution: Substitution): ConstrainedTerm =
    ConstrainedTerm(term.applySubstitution(substitution), constraints.map(_.applySubstitution(substitution)))

  def addConstraint(constraint: Constraint): ConstrainedTerm =
    this.copy(constraints = constraints + constraint)

  override def toString: String = toPrintString(false)

  override def toPrintString(colours: Boolean = true): String =
    s"${term.toPrintString(colours)} ${super.toPrintString(colours)}"
}