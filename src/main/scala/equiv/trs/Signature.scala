package equiv.trs

case class Signature(functions: Set[FunctionSymbol]) {
  def union(other: Signature) : Signature = Signature(functions ++ other.functions)
  def asMap : Map[String, FunctionSymbol] = functions.map{ f => f.name -> f }.toMap

  override def toString: String = {
    functions.toList.sortBy(_.typing.toString).mkString("\n")
  }
}

