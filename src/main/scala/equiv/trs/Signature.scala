package equiv.trs

case class Sort(name: String) {
  override def toString: String = name
}

object Sort {
  val Bool: Sort = Sort("Bool")
  val Int: Sort = Sort("Int")
}

case class Typing(input: List[Sort], output: Sort, isTheory: Boolean = false) {
  override def toString: String = {
    s"${if(input.nonEmpty) input.mkString(""," * ", " => ") else ""}$output"
  }
}

case class Infix(isLeft: Boolean, bindingStrength: Int)

case class FunctionSymbol(name: String, typing: Typing, infix: Option[Infix] = None) {
  override def toString: String = {
    s"$name : $typing"
  }
}

object FunctionSymbol {
  def `Int`(nr: Int): FunctionSymbol = FunctionSymbol(nr.toString, Typing(List.empty, Sort.Int, isTheory = true))
}

case class Signature(functions: Set[FunctionSymbol]) {
  def union(other: Signature) : Signature = Signature(functions ++ other.functions)
  def asMap : Map[String, FunctionSymbol] = functions.map{ f => f.name -> f }.toMap
}

case class Renaming(from: String, to: String)

case class Theory(signature: Signature, renaming: Set[Renaming]) {
  def union(other: Theory) : Theory = Theory(signature.union(other.signature), renaming ++ other.renaming)
}
