package equiv.utils

object OptionExtension {
  extension [A](option: Option[A]) {
    /** Print a string to the console if the current object is None.
     * @param string The string to print to the console
     * @return The current object, unaltered */
    def printOnNone(string: String): Option[A] = option match {
      case x@Some(_) => x
      case None => println(string); None
    }

    /** If the current object is [[None]], return the given value, otherwise return itself.
     * @param value Value to return if the current object is [[None]]
     * @return [[this]] if [[this]] is of the form [[Some]](...), or [[value]] otherwise */
    def getOnNone(value: Option[A]): Option[A] = option match {
      case x@Some(_) => x
      case None => value
    }
  }
}
