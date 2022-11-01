package equiv.trs

import equiv.trs.Term.{App, Substitution, Var}
import equiv.utils.{TermUtils, TheorySymbols}

case class Constraint(term: Term) {
  assert(term.sort == Sort.Bool)

  def applySubstitution(substitution: Substitution): Constraint = this.copy(term = term.applySubstitution(substitution))

  /** Split a constraint into a set of constraints, on the conjunctions. */
  def split(term2: Term = term): Set[Constraint] = {
    term2 match {
      case App(f, args) =>
        if f.name == TheorySymbols.and.name then
          return args.flatMap(split).toSet
    }
    Set(Constraint(term2))
  }

  /**
   * Check if the constraint is the assignment of a term to a variable, i.e. of the form `x = t` or `t = x`, where `x` is a variable and `t` a non-variable term.
   * @return The variable that is assigned if the constraint is of this form, [[None]] otherwise
   */
  def maybeVar: Option[Term.Var] = {
    term match {
      case App(FunctionSymbol(TermUtils.equalityFunctionSymbolName, _, _, _, _, _), List(l, r)) =>
        (l, r) match {
          case (v@Var(_, _), App(_, _)) => Some(v)
          case (App(_, _), v@Var(_, _)) => Some(v)
          case _ => None
      }
      case _ => None
    }
  }

  override def toString: String = toPrintString(false)

  def toPrintString(colours: Boolean = true): String = if this == TheorySymbols.constraintTrue then "" else s" [ ${term.toPrintString(colours)} ]"
}
