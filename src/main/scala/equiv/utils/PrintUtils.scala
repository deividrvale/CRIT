package equiv.utils

object PrintUtils {
  val nAryFunctionColour: String = Console.BLUE
  val zeroAryFunctionColour: String = Console.MAGENTA
  val variableColour: String = Console.GREEN
  val failureColour: String = Console.RED

  /** Append " failure." to the given string and colour it red. */
  def failureString(string: String): String = s"$failureColour$string failed.${Console.RESET}"
}
