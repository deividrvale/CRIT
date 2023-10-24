package equiv.trs

import equiv.trs.parsing.QuasiSystem

case class System(theory: String, logic: String, solver: String, signature: Signature, rules: Set[Rule], query: Option[Query] = None) {
  val definedSymbols: Set[FunctionSymbol] = rules.flatMap(_.rootFunc)
  
  override def toString: String = toPrintString(false)

  def toPrintString(colours: Boolean = true): String = {
    s"""SIGNATURE:
       |  ${signature.toString.replace("\n","\n  ")}
       |
       |RULES:
       |  ${rules.toList.map(_.toPrintString(colours)).sorted.mkString("\n  ")}
       |
       |QUERY:
       |  $query
       |""".stripMargin
  }
}
