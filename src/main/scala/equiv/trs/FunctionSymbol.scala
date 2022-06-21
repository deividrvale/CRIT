package equiv.trs

case class FunctionSymbol(name: String, typing: Typing, infix: Option[Infix] = None) {
  override def toString: String = {
    s"$name" // : $typing"
  }

  def toPrintString: String = {
    typing.input match {
      case List() => Console.GREEN + s"$name" + Console.RESET
      case _ => Console.RED + s"$name" + Console.RESET
    }
  }
}

object FunctionSymbol {
  def `Int`(nr: Int): FunctionSymbol = FunctionSymbol(nr.toString, Typing(List.empty, Sort.Int, isTheory = true))
}