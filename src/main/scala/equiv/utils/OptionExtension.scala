package equiv.utils

object OptionExtension {
  extension [A](option: Option[A]) {
    def printOnNone(string: String): Option[A] = option match {
      case x@Some(_) => x
      case None => println(string); None
    }

    def printOnNone(string: String, doPrint: Boolean): Option[A] = option match {
      case x@Some(_) => x
      case None => if doPrint then println(string); None
    }
  }
}
