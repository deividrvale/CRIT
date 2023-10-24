package equiv.trs.parsing

import equiv.ri.Equation
import equiv.trs.{Constraint, FunctionSymbol, Rule, Sort}
import equiv.utils.MapUtils
import equiv.trs.parsing.QuasiTerm.{App, InfixChain}

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

  def toRule(signature: Map[String, FunctionSymbol], variableSorts: Map[String, Sort], ignoreRootFuncAssert: Boolean = false): Rule = {
    Rule(
      left.toTerm(signature, variableSorts),
      right.toTerm(signature, variableSorts),
      constraint.map(t => Constraint(t.toTerm(signature, variableSorts)).split()).getOrElse(Set()),
      ignoreRootFuncAssert
    )
  }

  def toEquation(signature: Map[String, FunctionSymbol], variableSorts: Map[String, Sort]): Equation = {
    Equation(
      left.toTerm(signature, variableSorts),
      right.toTerm(signature, variableSorts),
      constraint.map(t => Constraint(t.toTerm(signature, variableSorts)).split()).getOrElse(Set())
    )
  }

  override def toString: String = {
    s"$left -> $right ${constraint.map { c => s"[ $c ]" }.getOrElse("")}"
  }
}
