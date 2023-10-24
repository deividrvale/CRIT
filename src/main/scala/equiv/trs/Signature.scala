package equiv.trs

import equiv.trs.parsing.QuasiSignature

case class Signature(functions: Set[FunctionSymbol]) {
  def union(other: Signature) : Signature = Signature(functions ++ other.functions)
  def asMap : Map[String, FunctionSymbol] = functions.map{ f => f.name -> f }.toMap

  override def toString: String = toPrintString(false)

  def toPrintString(colours: Boolean = true): String = {
    functions.toList.map(_.toPrintString(colours, true)).mkString("\n")
  }

  def toQuasiSignature: QuasiSignature = {
    QuasiSignature(functions.map(Left(_)))
  }
}

