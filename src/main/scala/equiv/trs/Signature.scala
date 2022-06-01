package equiv.trs

case class Sort(name: String)

object Sort {
  val Bool: Sort = Sort("Bool")
  val Int: Sort = Sort("Int")
}

case class Typing(input: List[Sort], output: Sort, isTheory: Boolean = false)

case class Infix(isLeft: Boolean, bindingStrength: Int)

case class FunctionSymbol(name: String, typing: Typing, infix: Option[Infix] = None)

object FunctionSymbol {
  def `Int`(nr: Int): FunctionSymbol = FunctionSymbol(nr.toString, Typing(List.empty, Sort.Int, isTheory = true))
}

case class Signature(functions: Set[FunctionSymbol])
