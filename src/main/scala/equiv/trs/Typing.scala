package equiv.trs

case class Typing(input: List[Sort], output: Sort, isTheory: Boolean = false, isVariadic: Boolean = false) {
  override def toString: String = {
    s"${if (input.nonEmpty) input.mkString("", " x ", (if (isVariadic) "*" else "") + " => ") else ""}$output"
  }

  def getSort(arg: Option[Int]): Option[Sort] = {
    arg match {
      case Some(argNr_) =>
        val argNr = if (isVariadic) Math.min(argNr_, input.length - 1) else argNr_
        if (0 <= argNr && argNr < input.length) Some(input(argNr)) else None
      case None => Some(output)
    }
  }
}
