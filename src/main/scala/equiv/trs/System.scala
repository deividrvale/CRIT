package equiv.trs

case class System(theory: String, logic: String, solver: String, signature: Signature, rules: Set[Rule]) {
  val definedSymbols: Set[FunctionSymbol] = rules.flatMap(_.rootFunc)
  
  override def toString: String = toPrintString(false)

  def toPrintString(colours: Boolean = true): String = {
    s"""SIGNATURE:
       |  ${signature.toString.replace("\n","\n  ")}
       |
       |RULES:
       |  ${rules.toList.map(_.toPrintString(colours)).sorted.mkString("\n  ")}
       |""".stripMargin
  }
}
