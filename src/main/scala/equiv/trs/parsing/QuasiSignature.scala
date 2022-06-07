package equiv.trs.parsing

import equiv.trs.FunctionSymbol

case class QuasiSignature(functions: Set[Either[FunctionSymbol, String]]) {
  def union(other: QuasiSignature): QuasiSignature = QuasiSignature(functions ++ other.functions)

  def asMap: Map[String, Either[FunctionSymbol, String]] = functions.map { f =>
    (f match
      case Left(symbol) => symbol.name
      case Right(name) => name
      ) -> f
  }.toMap

  def left: Set[FunctionSymbol] = functions.filter(_.isLeft).map(_.swap.getOrElse(throw new RuntimeException("Impossible!")))

  def leftAsMap: Map[String, FunctionSymbol] = left.map { f => f.name -> f }.toMap
}
