package equiv.utils

object PrintUtils {
  val nAryFunctionColour: String = Console.BLUE
  val zeroAryFunctionColour: String = Console.MAGENTA
  val variableColour: String = Console.GREEN
  val failureColour: String = Console.RED
  val successColour: String = Console.YELLOW
  val positionColour: String = Console.CYAN

  /** Map for certain function symbols that should be printed differently from their internal representation. */
  val functionSymbolPrintStrings: Map[String, String] = Map( (TheorySymbols.and.name, "/\\"), (TheorySymbols.or.name, "\\/"), (TheorySymbols.not.name, "¬") )

  /** Append " failure." to the given string and colour it red. */
  def failureString(string: String): String = s"$failureColour$string failed.${Console.RESET}"

  def failureColourString(string: String): String = s"$failureColour$string${Console.RESET}"

  def successColourString(string: String): String = s"$successColour$string${Console.RESET}"
}
