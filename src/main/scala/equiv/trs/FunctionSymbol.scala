package equiv.trs

case class FunctionSymbol(name: String, typing: Typing, infix: Option[Infix] = None) {
  override def toString: String = {
    s"$name : $typing"
  }
}

object FunctionSymbol {
  def `Int`(nr: Int): FunctionSymbol = FunctionSymbol(nr.toString, Typing(List.empty, Sort.Int, isTheory = true))
}