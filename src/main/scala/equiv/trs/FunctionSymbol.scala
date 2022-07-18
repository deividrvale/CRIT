package equiv.trs

import equiv.ri.ProofState
import equiv.utils.{PrintUtils, TermUtils, TheorySymbols}

case class FunctionSymbol(name: String, typing: Typing, isTheory: Boolean = false, isValue: Boolean = false, infix: Option[Infix] = None, isTemporary: Boolean = false) {
  assert( typing.input.isEmpty || !isValue,
  s"Function symbol $name is a value, but has arity > 0." ) // assert that values have arity 0

  assert( !isTemporary || name != TermUtils.reservedFunctionSymbol,
  s"Function symbol name ${TermUtils.reservedFunctionSymbol} is protected and cannot be used.")

  def isConstructor(definedSymbols: Set[FunctionSymbol]): Boolean = !definedSymbols.contains(this) && (!isTheory || isValue)

  override def toString: String = toPrintString(false)

  def toPrintString(colours: Boolean = true, printTyping: Boolean = false): String = {
    (if colours then
        (typing.input match {
          case List() => PrintUtils.zeroAryFunctionColour
          case _ => PrintUtils.nAryFunctionColour 
        }) + s"${toPrintName(name)}" + Console.RESET
      else
        s"$name")
    + (if printTyping then s" : ${typing.toPrintString()}" else "")
  }

  private def toPrintName(string: String): String = {
    if PrintUtils.functionSymbolPrintStrings.contains(string) then PrintUtils.functionSymbolPrintStrings(string)
    else string
  }
}

object FunctionSymbol {
  def `Int`(nr: Int): FunctionSymbol = FunctionSymbol(nr.toString, Typing(List.empty, Sort.Int), isTheory = true, isValue = true)
}