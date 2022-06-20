package equiv.trs

import equiv.trs.Term.*
import equiv.utils.TermUtils

case class ConstrainedTerm(term: Term, constraints: Set[Constraint]) extends ConstrainedObject(constraints) {
  /** @return A list of variables that occur in this constrained term */
  def vars(ct: ConstrainedTerm): Set[Var] = ct.term.vars ++ constraints.flatMap(_.term.vars)

  /** Substitute a subterm with `replacement` at the given `position`.
   * @param position Position of substitution as a list of Ints
   * @param replacement Term to substitute */
  def substituteAtPos(position: Position, replacement: Term): ConstrainedTerm =
    ConstrainedTerm(term.substituteAtPos(position, replacement), constraints)

  def rewriteAtPos(position: Position, replacement: Term, substitution: Substitution): ConstrainedTerm =
    ConstrainedTerm(term.rewriteAtPos(position, replacement, substitution), constraints)

  override def toString: String = {
    s"$term ${super.toString}"
  }
}