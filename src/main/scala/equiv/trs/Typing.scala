package equiv.trs

/**
 * @param input Sort of the arguments of the function symbol
 * @param output Output sort
 * @param isVariadic Whether the function symbol supports an arbitrary number of arguments
 */
case class Typing(input: List[Sort], output: Sort, isVariadic: Boolean = false) {
  override def toString: String = toPrintString(false)
  
  def toPrintString(colours: Boolean = true): String = {
    s"${if (input.nonEmpty) input.map(_.toPrintString()).mkString("", " x ", (if (isVariadic) "*" else "") + " => ") else ""}${output.toPrintString()}"
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
