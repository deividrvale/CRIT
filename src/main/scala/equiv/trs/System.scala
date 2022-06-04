package equiv.trs

case class System(theory: String, logic: String, solver: String, signature: Signature, rules: Set[Rule]) {
  override def toString: String = {
    s"""SIGNATURE:
       |  ${signature.toString.replace("\n","\n  ")}
       |
       |RULES:
       |  ${rules.toList.map(_.toString).sorted.mkString("\n  ")}
       |""".stripMargin
  }
}
