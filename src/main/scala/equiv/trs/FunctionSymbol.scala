package equiv.trs

import equiv.ri.ProofState
import equiv.utils.PrintUtils

case class FunctionSymbol(name: String, typing: Typing, isTheory: Boolean = false, isValue: Boolean = false, infix: Option[Infix] = None) {
  def isConstructor(definedSymbols: Set[FunctionSymbol]): Boolean = !definedSymbols.contains(this) && (!isTheory || isValue)

  override def toString: String = toPrintString(false)

  def toPrintString(colours: Boolean = true, printTyping: Boolean = false): String = {
    (if colours then
        (typing.input match {
          case List() => PrintUtils.zeroAryFunctionColour
          case _ => PrintUtils.nAryFunctionColour 
        }) + s"$name" + Console.RESET
      else
        s"$name") 
    + (if printTyping then s" : ${typing.toPrintString()}" else "")
  }
}

object FunctionSymbol {
  def `Int`(nr: Int): FunctionSymbol = FunctionSymbol(nr.toString, Typing(List.empty, Sort.Int), isTheory = true, isValue = true)
}