package equiv.trs

case class Signature(functions: Set[FunctionSymbol]) {
  def union(other: Signature) : Signature = Signature(functions ++ other.functions)
  def asMap : Map[String, FunctionSymbol] = functions.map{ f => f.name -> f }.toMap

  override def toString: String = toPrintString(false)

  def toPrintString(colours: Boolean = true): String = {
    functions.toList.sortBy(_.typing.toString).map(_.toPrintString(colours)).mkString("\n")
  }
}

