package equiv.trs

import equiv.utils.Print

case class FunctionSymbol(name: String, typing: Typing, infix: Option[Infix] = None) {
  override def toString: String = toPrintString(false)

  def toPrintString(colours: Boolean = true, printTyping: Boolean = false): String = {
    (if colours then
        (typing.input match {
          case List() => Print.zeroAryFunctionColour
          case _ => Print.nAryFunctionColour 
        }) + s"$name" + Console.RESET
      else
        s"$name") 
    + (if printTyping then s" : ${typing.toPrintString()}" else "")
  }
}

object FunctionSymbol {
  def `Int`(nr: Int): FunctionSymbol = FunctionSymbol(nr.toString, Typing(List.empty, Sort.Int, isTheory = true))
}