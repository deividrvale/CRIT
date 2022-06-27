package equiv.trs.parsing

import equiv.trs.{Constraint, FunctionSymbol, Rule, Sort}
import equiv.utils.{MapUtils, TermUtils}
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

  def toRule(signature: Map[String, FunctionSymbol], variableSorts: Map[String, Sort]): Rule = {
    Rule(
      left.toTerm(signature, variableSorts),
      right.toTerm(signature, variableSorts),
      constraint.map(splitQuasiTerm(_, signature, variableSorts)).getOrElse(Set())
    )
  }

  private def splitQuasiTerm(quasiTerm: QuasiTerm, signature: Map[String, FunctionSymbol], variableSorts: Map[String, Sort]): Set[Constraint] = {
    quasiTerm match {
      case App(fun, args) => 
        if fun == TermUtils.conjunctionSymbol then 
          return args.flatMap(splitQuasiTerm(_, signature, variableSorts)).toSet
      case InfixChain(head, tail) => 
        if tail.head._1 == TermUtils.conjunctionSymbol then
          return (head :: tail.map(_._2)).flatMap(splitQuasiTerm(_, signature, variableSorts)).toSet
    }
    return Set(Constraint(quasiTerm.toTerm(signature, variableSorts)))
  }

  override def toString: String = {
    s"$left -> $right ${constraint.map { c => s"[ $c ]" }.getOrElse("")}"
  }
}
