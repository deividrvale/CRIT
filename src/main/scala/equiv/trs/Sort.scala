package equiv.trs

case class Sort(name: String, isTheory: Boolean = true) {
  override def toString: String = toPrintString(false)

  def toPrintString(colours: Boolean = true): String = name
}

object Sort {
  // for polymorphic functions
  val Any: Sort = Sort("?")

  val Bool: Sort = Sort("Bool")
  val Int: Sort = Sort("Int")
}