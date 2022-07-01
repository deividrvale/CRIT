package equiv.utils

object OptionExtension {
  extension [A](option: Option[A]) {
    def printOnNone(string: String): Option[A] = option match {
      case x@Some(_) => x
      case None => println(string); None
    }
  }
}
