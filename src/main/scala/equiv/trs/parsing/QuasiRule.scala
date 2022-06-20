package equiv.trs.parsing

import equiv.trs.{Constraint, FunctionSymbol, Rule, Sort}
import equiv.utils.{MapUtils, TermUtils}

case class QuasiRule(left: QuasiTerm, right: QuasiTerm, constraint: Option[QuasiTerm]) {
  def infixOperators: Set[String] = {
    left.infixOperators ++ right.infixOperators ++ constraint.toSet.flatMap { term => term.infixOperators }
  }

  def functionSymbols: Set[(String, Int)] = {
    left.functionSymbols ++ right.functionSymbols ++ constraint.toSet.flatMap { term => term.functionSymbols }
  }

  def infix2app(signature: Map[String, FunctionSymbol]): QuasiRule = {
    QuasiRule(left.infix2app(signature), right.infix2app(signature), constraint.map(_.infix2app(signature)))
  }

  def toRule(signature: Map[String, FunctionSymbol], variableSorts: Map[String, Sort]): Rule = {
    Rule(
      left.toTerm(signature, variableSorts),
      right.toTerm(signature, variableSorts),
      constraint.map { c => Set(Constraint(c.toTerm(signature, variableSorts))) }.getOrElse(Set())
    )
  }

  override def toString: String = {
    s"$left -> $right ${constraint.map { c => s"[ $c ]" }.getOrElse("")}"
  }
}
