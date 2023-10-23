package equiv.trs.parsing

import equiv.trs.FunctionSymbol

case class QuasiSignature(functions: Set[Either[FunctionSymbol, String]]) {
  def union(other: QuasiSignature): QuasiSignature = {
    val aMap = asMap
    val bMap = other.asMap
    val names = aMap.keySet ++ bMap.keySet
    QuasiSignature(names.map{ name =>
      (aMap.get(name),bMap.get(name)) match {
        case (Some(Left(symbol)), _) => Left(symbol)
        case (_, Some(Left(symbol))) => Left(symbol)
        case (Some(Right(name)), _) => Right(name)
        case (_, Some(Right(name))) => Right(name)
        case (None, None) => throw new RuntimeException("impossible")
      }
    })
  }
  
  def asMap: Map[String, Either[FunctionSymbol, String]] = functions.map { f =>
    (f match
      case Left(symbol) => symbol.name
      case Right(name) => name
      ) -> f
  }.toMap

  def left: Set[FunctionSymbol] = functions.filter(_.isLeft).map(_.swap.getOrElse(throw new RuntimeException("Impossible!")))

  def leftAsMap: Map[String, FunctionSymbol] = left.map { f => f.name -> f }.toMap

  def getFunctionSymbols: Set[FunctionSymbol] = functions.filter(_ match { case Left(_) => true; case Right(_) => false }).map { case Left(f) => f }
}
